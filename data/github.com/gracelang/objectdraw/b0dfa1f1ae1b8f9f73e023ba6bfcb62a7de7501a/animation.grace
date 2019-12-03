dialect "standard"
import "timer" as timer

type NumberBlock = Function0⟦Number⟧

type Animator = {
    // type of object that can simulate parallel animations

    while (condition:Predicate0) pausing (pauseTime:Number)
          do (block:Procedure0) -> Done
    // Repeatedly execute block while condition is true

    while (condition:Predicate0) pausing (pauseTime:Number)
          do (block:Procedure0) finally (endBlock:Procedure0) -> Done
    // Repeatedly execute block while condition is true, pausing pauseTime
    // between iterations. When condition fails, execute endBlock.

    while (condition:Predicate0) pauseVarying (timeBlock:NumberBlock)
          do (block:Procedure0) -> Done
    // Repeatedly execute block while condition is true, pausing a variable
    // amount of time (obtained by evaluating timeBlock) between iterations.
    // When condition fails, execute endBlock.

    for⟦T⟧ (range':Collection⟦T⟧) pausing (pauseTime:Number) do (block:Procedure1⟦T⟧) -> Done
    // Repeatedly execute block while condition is true

    for⟦T⟧ (range':Collection⟦T⟧) pausing (pauseTime:Number) do (block:Procedure1⟦T⟧)
          finally (endBlock:Procedure0) -> Done
    // Repeatedly execute block while condition is true
    // when condition fails, execute endBlock.

}

method while(condition:Predicate0) pausing (pauseTime:Number)
                  do (block:Procedure0) -> Done {
    // Repeatedly execute block while condition is true

    def id:Number = timer.every (pauseTime) do {
        if (condition.apply) then {
            block.apply
        } else {
            timer.stop (id)
        }
    }
}

method while (condition:Predicate0) pausing (pauseTime:Number)
          do (block:Procedure0) finally (endBlock:Procedure0) -> Done {
    // Repeatedly execute block while condition is true, pausing by pauseTime
    // between iterations. When condition fails, execute endBlock.

    def id:Number = timer.every(pauseTime)do{
        if (condition.apply) then {
            block.apply
        } else {
            timer.stop(id)
            endBlock.apply
        }
    }
}

method while(condition:Predicate0) pauseVarying (timeBlock:NumberBlock)
          do (block:Procedure0)  -> Done {
    // Repeatedly execute block while condition is true, pausing by
    // timeBlock.apply between iterations.

    if (condition.apply) then {
        block.apply
        timer.after(timeBlock.apply) do {
            while (condition) pauseVarying (timeBlock) do (block)
        }
    }
}

method for⟦T⟧(range':Collection⟦T⟧) pausing (pauseTime:Number)
          do (block:Procedure1⟦T⟧) -> Done {
    // Repeatedly execute block for each value in range', pausing pauseTime between
    // iterations; block should take an element of range' as an argument.

    def it = range'.iterator
    while {it.hasNext} pausing (pauseTime) do { block.apply(it.next) }
}

method for⟦T⟧ (range':Collection⟦T⟧) pausing (pauseTime:Number)
         do (block:Procedure1⟦T⟧) finally(endBlock:Procedure0) -> Done {
    // Repeatedly execute block for each value in range', pausing pauseTime between
    // iterations; block should take a T object as a parameter.  When range'
    // is exhausted, execute endBlock.

    def it:Iterator⟦T⟧ = range'.iterator
    while {it.hasNext} pausing (pauseTime) do { block.apply(it.next) }
         finally(endBlock)
}
