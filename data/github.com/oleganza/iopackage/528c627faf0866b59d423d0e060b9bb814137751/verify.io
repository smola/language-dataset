#!/usr/bin/env io

Object clone do(
  
  Results := List clone do(
    shortFormat := method(
      str := "" asMutable
      foreach(result, 
        str appendSeq(result shortFormat)
      )
      str
    )
    
    failureSummary := method(
      str := "" asMutable
      foreach(result, 
        if(result exception,
          str appendSeq(result failureSummary) appendSeq("\n")
        )
      )
      str
    )
  )
  Result := Object clone do(
    methodName ::= nil
    exception  ::= nil
    
    shortFormat := method(
      if(exception, 
        "F", "."
      )
    )
    
    failureSummary := method(
      methodName asMutable appendSeq(": ") appendSeq(exception asString)
    )
  )
  
  Failure := Exception clone do(
    predicate ::= nil
    asString := method(
      predicate asString asMutable appendSeq(": ") appendSeq(error asString)
    )
  )
  
  run := method(
    results := Results clone
    self slotNames foreach(slotName,
      slot := self getSlot(slotName)
      if(slot hasSlot("argumentNames"), // slot could not be a method
        if(slot argumentNames isEmpty, results append(_performTest(slotName)))
      )
    )
    results
  )
  
  _performTest := method(slotName,
    self println
    e := try(self getSlot(slotName) call)
    Result clone setMethodName(slotName) setException(e) 
  )
  
  verify := method(
    call message arguments foreach(predicate,
      r := nil
      e := try(r = doMessage(predicate))
      if(e,
        Failure clone setPredicate(predicate) raise("exception raised"),
        if(r not, 
          Failure clone setPredicate(predicate) raise("check failed"),
          r
        )
      )
    )
  )
  
  
  //
  // Tests
  //
  if(isLaunchScript,   // we include this inside object so that it can be returned out of the file
    clone do(
      tests := proto clone
    
      tests do(
        arithmetics := method(
          verify(1 + 2 == 3)
          verify(4 + 2 == 7)
          "this should not be executed!" println
        )
        logic := method(
          verify(true == true)
          verify(false == false)
          verify(nil isNil)
          verify(nil not)
        )
      )
    
      results := tests run
      results shortFormat println
      results failureSummary println
    )
  ) // isLaunchScript
) 
