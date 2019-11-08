import ceylon.whole {
    zero,
    one,
    Whole,
    two
}
import com.vasileff.fc.core {
    Monad,
    wholePlusMonoid
}
import com.vasileff.fc.trampoline {
    Call,
    Bounce,
    Return,
    bounceTypeClass
}

Bounce<Boolean> even(Integer x, Boolean alwaysBounce = false)
    =>  if (x == 0) then
            Return(true)
        else if (alwaysBounce || 1000.divides(x)) then
            Call(() => odd(x - 1, alwaysBounce))
        else
            odd(x - 1, alwaysBounce);

Bounce<Boolean> odd(Integer x, Boolean alwaysBounce = false)
    =>  if (x == 0) then
            Return(false)
        else if (alwaysBounce || 1000.divides(x)) then
            Call(() => odd(x - 1, alwaysBounce))
        else
            even(x - 1, alwaysBounce);

Bounce<Boolean> flip(Boolean b)
    =>  Return(!b);

Bounce<Whole> fact(Integer x) {
    Bounce<Whole> step(Integer x, Whole acc = one)()
        =>  if (x <= 1)
            then Return(acc)
            else Call(step(x - 1, acc.timesInteger(x)));

    return step(x)();
}

Bounce<Whole> fib(Integer x) {
    Bounce<Whole> step(Integer x, Whole a = zero, Whole b = one)()
        =>  if (x <= 2)
            then Return(a + b)
            else Call(step(x - 1, b, a + b));

    return step(x)();
}

shared
void runTrampolineTest() {
    print(fact(6).map(Whole.integer).flatMap(fib).result);
    print(fib(20).map(Whole.integer).flatMap(fact).result);

    print(even(1M).flatMap(flip).result);
    print(even(1M).map((x) => !x).result);
}

shared
void bounceTypeClassExamples() {
    function quadrupleWithLift<Container>(
            Monad<Container> monad,
            Container<Whole> ints)
            given Container<out E>
        =>  let (double = monad.lift(two.times))
            double(double(ints));

    print(quadrupleWithLift(bounceTypeClass, fact(6)).result);

    value double = two.times;
    value doubleBounce = bounceTypeClass.lift(double);
    print(doubleBounce(fact(2)).result);
    print(doubleBounce(bounceTypeClass.unit(two)).result);

    print((bounceTypeClass.wrap(fact(10))
            .map(two.times).unwrapped.result));

    // use fold to get a value!
    print((bounceTypeClass.wrap(fact(10))
            .map(two.times).fold(wholePlusMonoid)));
}
