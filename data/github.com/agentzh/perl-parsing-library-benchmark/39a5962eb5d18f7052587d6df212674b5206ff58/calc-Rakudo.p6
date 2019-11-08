#!/usr/bin/env perl6

use v6;

my grammar Arith {
    rule TOP {
        | <.ws> <expr> { make $<expr>.made }
        | { self.panic("Bad expression") }
    }

    rule expr {
        | <term> + % <add-op>   { self.do_calc($/, $<term>, $<add-op>) }
        | { self.panic("Bad expression") }
    }

    token add-op {
        | < + - >
        #| { self.panic($/, "Bad addition/substraction operator") }
    }

    rule term {
        | <factor> + % <mul-op>  { make self.do_calc($/, $<factor>, $<mul-op>) }
        | { self.panic($/, "Bad term") }
    }

    token mul-op {
        | < * / >
        #| { self.panic($/, "Bad multiplication/division operator") }
    }

    rule factor {
        | <atom> + % '^'
            {
                make [**] map { $_.made }, @<atom>;
            }
        | { self.panic($/, "Bad factor") }
    }

    rule atom {
        | <number> { make +$<number> }
        | '(' ~ ')' <expr> { make $<expr>.made }
        | { self.panic($/, "Bad atom") }
    }

    rule number {
        <.sign> ? <.pos-num>
        | { self.panic($/, "Bad number") }
    }

    token sign { < + - > }
    token pos-num {
        | <.digit>+ [ \. <digit>+ ]?
        | \. <.digit>+
        | { self.panic($/, "Bad number") }
    }

    method do_calc($/, $values, $ops) {
        my $res = $values.shift.made;
        while ($values.elems) {
            my $op = $ops.shift;
            my $other = $values.shift.made;

            given $op {
                when '+' { $res += $other; }
                when '-' { $res -= $other; }
                when '*' { $res *= $other; }
                default {  # when '/'
                    $res /= $other;
                }
            }
        }
        make $res;
    }

    method panic($/, $msg) {
        my $c = $/.CURSOR;
        my $pos := $c.pos;
        die "$msg found at pos $pos";
    }
}

my $input = (@*ARGS[0] // slurp);

my $begin = now;
try Arith.parse($input);
if $! {
    say "Parse failed: ", $!.message;

} elsif $/ {
    my $elapsed = now - $begin;
    printf "Elapsed: %.03f sec.\n", $elapsed;
    say "Result: ", $();

} else {
    say "Parse failed.";
}
