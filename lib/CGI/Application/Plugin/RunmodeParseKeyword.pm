package CGI::Application::Plugin::RunmodeParseKeyword;

use warnings;
use strict;

=head1 NAME

CGI::Application::Plugin::RunmodeParseKeyword - Declare runmodes using Parse::Keyword

=cut

use base 'Exporter';
our @EXPORT = qw(runmode errormode startmode);
use Carp qw(croak);
use Sub::Name 'subname';
use Parse::Keyword {};

sub import {
    my $caller = caller;
    my $class = shift;
    my %args = @_;
    my $inv = delete $args{invocant} || '$self';

    Parse::Keyword->import( { runmode   => sub { my ($kw) = @_; parse_mode($kw, $inv); } } );
    Parse::Keyword->import( { errormode => sub { my ($kw) = @_; parse_mode($kw, $inv); } } );
    Parse::Keyword->import( { startmode => sub { my ($kw) = @_; parse_mode($kw, $inv); } } );

    local $Exporter::ExportLevel = 1;
    $class->SUPER::import(@EXPORT);
}

sub runmode { @_ ? $_[0] : () }
sub errormode { @_ ? $_[0] : () }
sub startmode { @_ ? $_[0] : () }

my %REGISTRY;
sub _setup_runmode {
    my ($pkg, $name, $code) = @_;
    $pkg->add_callback( init => sub { $_[0]->run_modes([ $name ]) } );
}
sub _setup_startmode {
    my ($pkg, $name, $code) = @_;
    no strict 'refs'; no warnings 'uninitialized';
    # compile time check
    croak "start mode redefined (from $REGISTRY{$pkg}{start_mode_installed})" if $REGISTRY{$pkg}{start_mode_installed};
    $pkg->add_callback(
        init => sub {
            # run time check
            return if exists $_[0]->{__START_MODE_SET_BY_RUNMODEDECLARE};
            $_[0]->run_modes( [$name] );
            $_[0]->start_mode($name);
            $_[0]->{__START_MODE_SET_BY_RUNMODEDECLARE} = 1;
        }
    );
    $REGISTRY{$pkg}{start_mode_installed} = join '::', $pkg, $name;
}
sub _setup_errormode {
    my ($pkg, $name, $code) = @_;
    no strict 'refs'; no warnings 'uninitialized';
    croak "error mode redefined (from $REGISTRY{$pkg}{error_mode_installed})" if $REGISTRY{$pkg}{error_mode_installed};
    $pkg->add_callback(
        init => sub {
            return if exists $_[0]->{__ERROR_MODE_SET_BY_RUNMODEDECLARE};
            $_[0]->error_mode($name);
            $_[0]->{__ERROR_MODE_SET_BY_RUNMODEDECLARE} = 1;
        }
    );
    $REGISTRY{$pkg}{error_mode_installed} = join '::', $pkg, $name;
}

sub parse_mode {
    my ($keyword, $invocant) = @_;

    my $name = parse_name();
    my $sig  = parse_signature($invocant);
    my $attr = parse_attributes();
    my $body = parse_body($sig);

    if (defined $name) {
        my $full_name = join('::', compiling_package, $name);
        {
            no strict 'refs';
            *$full_name = subname $full_name, $body;
            if ($attr) {
                use attributes ();
                attributes->import(compiling_package, $body, $_) for @$attr;
            }
            my $setup = '_setup_' . $keyword;
            $setup->(compiling_package, $name, $body);

        }
        return (sub {}, 1);
    }
    else {
        return (sub { $body }, 0);
    }
}

my $start_rx = qr/^[\p{ID_Start}_]$/;
my $cont_rx  = qr/^\p{ID_Continue}$/;

sub parse_name {
    my $name = '';

    lex_read_space;

    my $char_rx = $start_rx;

    while (1) {
        my $char = lex_peek;
        last unless length $char;
        if ($char =~ $char_rx) {
            $name .= $char;
            lex_read;
            $char_rx = $cont_rx;
        }
        else {
            last;
        }
    }

    return length($name) ? $name : undef;
}

sub parse_signature {
    my ($invocant_name) = @_;
    lex_read_space;

    return unless lex_peek eq '(';

    my @attr = ();

    lex_read;
    lex_read_space;

    if (lex_peek eq ')') {
        lex_read;
        return;
    }

    my $seen_slurpy;
    my @vars = ({ index => 0, name => $invocant_name });
    while ((my $sigil = lex_peek) ne ')') {
        my $var = {};
        die "syntax error"
            unless $sigil eq '$' || $sigil eq '@' || $sigil eq '%';
        die "Can't declare parameters after a slurpy parameter"
            if $seen_slurpy;

        $seen_slurpy = 1 if $sigil eq '@' || $sigil eq '%';

        lex_read;
        lex_read_space;
        my $name = parse_name(0);
        lex_read_space;

        $var->{name} = "$sigil$name";

        if (lex_peek eq '=') {
            lex_read;
            lex_read_space;
            $var->{default} = parse_arithexpr;
        }

        $var->{index} = @vars - 1;

        if (lex_peek eq ':') {
            $vars[0] = $var;
            lex_read;
            lex_read_space;
            next;
        }

        push @vars, $var;

        die "syntax error"
            unless lex_peek eq ')' || lex_peek eq ',';

        if (lex_peek eq ',') {
            lex_read;
            lex_read_space;
        }
    }

    lex_read;

    return \@vars;
}

# grabbed these two functions from
# https://metacpan.org/release/PEVANS/XS-Parse-Keyword-0.22/source/hax/lexer-additions.c.inc#L74
sub parse_attribute {
    my $name = parse_name;
    if (lex_peek ne '(') {
        return $name;
    }
    $name .= lex_peek;
    lex_read;
    my $count = 1;
    my $c = lex_peek;
    while($count && length $c) {
        if($c eq '(') {
            $count++;
        }
        if($c eq ')') {
            $count--;
        }
        if($c eq '\\') {
            # The next char does not bump count even if it is ( or );
            # the \\ is still captured
            #
            $name .= $c;
            lex_read;
            $c = lex_peek;
            if(! length $c) {
                goto unterminated;
            }
        }

        # Don't append final closing ')' on split name/val
        $name .= $c;
        lex_read;

        $c = lex_peek;
    }

    if(!length $c) {
        return;
    }

    return $name;

unterminated:
    croak("Unterminated attribute parameter in attribute list");
    return;
}

sub parse_attributes {
    lex_read_space;
    return unless lex_peek eq ':';
    lex_read;
    lex_read_space;
    my @attrs;
    while (my $attr = parse_attribute) {
        push @attrs, $attr;
        lex_read_space;
        if (lex_peek eq ':') {
            lex_read;
            lex_read_space;
        }
    }

    return \@attrs;
}

sub parse_body {
    my $sigs = shift;
    my $body;

    lex_read_space;

    if (lex_peek eq '{') {
        local $CAPRPK::{'DEFAULTS::'};
        if ($sigs) {
            lex_read;

            my $preamble = '{';

            # invocant
            my $inv = shift @$sigs;
            $preamble .= "my $inv->{name} = shift;";

            # arguments / query params
            my @names = map { $_->{name} } @$sigs;
            $preamble .= 'my (' . join(', ', @names) . ') = @_;';

            for my $name (@names) {
                my $s = substr($name,0,1);
                my $n = substr($name,1);
                if ($s eq '$') {
                    my $p = $inv->{name} . '->param("' . $n . '")';
                    $preamble .= $name . ' = ' . $p . ' unless ' . ( $s eq '$' ? 'defined ' : 'scalar ') . $name . ';';
                }
                my $p = $inv->{name} . '->query->' . ($s eq '@' ? 'multi_param' : 'param') . '("' . $n . '")';
                $preamble .= $name . ' = ' . $p . ' unless ' . ( $s eq '$' ? 'defined ' : 'scalar ') . $name . ';';
                if ($s eq '@') {
                    my $p = $inv->{name} . '->query->' . ($s eq '@' ? 'multi_param' : 'param') . '("' . $n . '[]")';
                    $preamble .= $name . ' = ' . $p . ' unless ' . ( $s eq '$' ? 'defined ' : 'scalar ') . $name . ';';
                }
            }

            my $index = 0;
            for my $var (grep { defined $_->{default} } @$sigs) {
                {
                    no strict 'refs';
                    *{ 'CAPRPK::DEFAULTS::default_' . $index } = sub () {
                        $var->{default}
                    };
                }
                $preamble .= $var->{name} . ' = CAPRPK::DEFAULTS::default_' . $index . '->()' . ' unless ' . $var->{name} . ';';

                $index++;
            }

            # warn $preamble . $/;
            lex_stuff($preamble);
        }
        $body = parse_block;
    }
    else {
        die "syntax error";
    }
    return $body;
}

1;
