//
// Copyright (c) 2017, chunquedong
// Licensed under the Apache Licene 2.0
// History:
//   2017-7-1  Jed Young  Creation
//

class StrUtilTest : Test {

  Void testSplit() {
    str := "123::abcd::ef::gh"
    vs := StrUtil.split(str, "::", 2)
    verifyEq(vs, ["123", "abcd::ef::gh"])
  }

  Void testSplit2() {
    str := "::abcd::ef::"
    vs := StrUtil.split(str, "::", 5)
    verifyEq(vs, ["", "abcd", "ef", ""])
  }

  Void testSplit3() {
    str := ""
    vs := StrUtil.split(str, "", 5)
    verifyEq(vs, [""])
  }

  Void testSplit4() {
    str := ",a,b,c"
    vs := StrUtil.split(str, ",", 2)
    verifyEq(vs, ["", "a,b,c"])

    vs2 := StrUtil.split(str, ",", 3)
    verifyEq(vs2, ["", "a", "b,c"])
  }

  Void testSplit5() {
    str := "a,"
    vs := StrUtil.split(str, ",", 2)
    verifyEq(vs, ["a", ""])
  }

  Void testExtractPart() {
    str := "123::abcd::ef::gh"
    vs := StrUtil.extractPart(str, "::", "f:")
    verifyEq(vs, "abcd::e")
  }

  Void testExtractPart2() {
    str := "123::abcd"
    vs := StrUtil.extractPart(str, null, ":")
    verifyEq(vs, "123")

    vs2 := StrUtil.extractPart(str, ":", null)
    verifyEq(vs2, ":abcd")
  }

  Void testExtractPart3() {
    str := "123::abcd"
    vs := StrUtil.extractPart(str, ":", ",")
    verifyEq(vs, null)

    vs2 := StrUtil.extractPart(str, ",", ":")
    verifyEq(vs2, null)
  }
}

