xquery version "3.1";

module namespace test = 'http://basex.org/modules/xqunit-tests';

import module namespace ninetynine = "http://xquery.ninja/ninetynine"
  at "problem-17.xqm";

declare %unit:test function test:test1() {
  unit:assert-equals(ninetynine:split(array { 1 to 5 }, 0), [[], [1, 2, 3, 4, 5]])
};

declare %unit:test function test:test2() {
  unit:assert-equals(ninetynine:split(array { 1 to 5 }, 2), [[1, 2], [3, 4, 5]])
};

declare %unit:test function test:test3() {
  unit:assert-equals(ninetynine:split(array { 1 to 5 }, 3), [[1, 2, 3], [4, 5]])
};

declare %unit:test function test:test4() {
  unit:assert-equals(ninetynine:split(array { 1 to 5 }, 4), [[1, 2, 3, 4], [5]])
};

declare %unit:test function test:test5() {
  unit:assert-equals(ninetynine:split(array { 1 to 5 }, 5), [[1, 2, 3, 4, 5], []])
};

declare %unit:test function test:test6() {
    unit:assert-equals(ninetynine:split(array { 1 to 5 }, 6), [[1, 2, 3, 4, 5], []])
};

declare %unit:test function test:test7() {
    unit:assert-equals(ninetynine:split(array { 1 to 5 }, -1), [[], [1, 2, 3, 4, 5]])
};

declare %unit:test function test:test8() {
    unit:assert-equals(ninetynine:split(["aab", "b", "c", "aa"], 2), [[ "aab", "b"],["c", "aa" ]])
};
