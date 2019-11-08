:Namespace GAP_Tests

    (⎕IO ⎕ML)←0 1
    assert←{⍵:⍵ ⋄ 'Assertion failed'⎕SIGNAL 11}

      expecterror←{
          0::⎕SIGNAL(⍺≡⊃⎕DMX.DM)↓11
          z←⍺⍺ ⍵
          ⎕SIGNAL 11
      }


      RunAll←{
          ⎕←'Testing GAP'
          tests←{⍵/⍨(⊂'test_')∊⍨5↑¨⍵}⎕NL-3
          ⍵∘run¨tests
      }

      run←{
          ⍞←⍵
          ⍞←('...OK',⎕UCS 10)⊣(⍎⍵)⍺
      }

      ls←{
          ⎕ML←1
    ⍝ ⍵ ←→ filepath
    ⍝ ⍺ ←→ recurse flag
          ⍺←0
          r←⍉↑1 0 ⎕NINFO⍠1⊢⍵,'/*'
          ⍺=0:r
          ~∨/f←1=⊣/r:r
          r⍪⊃⍪/⍺ ∇¨f/⊢/r
      }

      rm←{
          r←1 ls ⍵
          x←1 ⎕NDELETE¨(2=⊣/r)/⊢/r
          x←1 ⎕NDELETE¨(1=⊣/r)/⊢/r
          x←1 ⎕NDELETE ⍵
          0
      }

    :Section Tests
      test_fsenc←{
          r←assert'.0'≡#.GAP.fsenc''
          r←assert'a.0'≡#.GAP.fsenc'a'
          r←assert'A.1'≡#.GAP.fsenc'A'
          r←assert'b_io.0'≡#.GAP.fsenc'b_io'
          r←assert'B_IO.B'≡#.GAP.fsenc'B_IO'
          r←assert'∆Syntax.20'≡#.GAP.fsenc'∆Syntax'
          0
      }

      test_fsdec←{
          r←assert''≡#.GAP.fsdec'.0'
          r←assert(,'a')≡#.GAP.fsdec'a.0'
          r←assert(,'A')≡#.GAP.fsdec'a.1'
          r←assert'b_io'≡#.GAP.fsdec'b_io.0'
          r←assert'B_IO'≡#.GAP.fsdec'b_io.B'
          r←assert'∆Syntax'≡#.GAP.fsdec'∆syNTax.20'
          r←assert'∆syNTax'≡#.GAP.fsdec'∆syNTax'
          0
      }
      test_nsProps←{
          x←3 ⎕MKDIR⊢tmp←'./tmp'
          ns←⎕NS''
          ns.(⎕IO ⎕ML ⎕PP)←0 3 11
          x←ns #.GAP.fixFolder tmp
          ns2←⎕NS''
          x←ns2 #.GAP.getProps tmp
          r←assert⊃≡/(ns ns2).(⎕IO ⎕ML ⎕PP)
          x←rm tmp
          0
      }
      test_a2ason←{
          r←assert'"1"'≡#.GAP.a2ason'1'
          r←assert'[1,6,"123456"]'≡#.GAP.a2ason'123456'
          r←assert'[2,2,3,"123456"]'≡#.GAP.a2ason 2 3⍴'123456'
          r←assert'[1,2,[1,3,"123"],[1,4,"4563"]]'≡#.GAP.a2ason'123' '4563'
          r←assert(,'1')≡#.GAP.a2ason 1
          r←assert'[1,6,1,2,3,4,5,6]'≡#.GAP.a2ason 1 2 3 4 5 6
          r←assert'[2,2,3,1,2,3,4,5,6]'≡#.GAP.a2ason 2 3⍴1 2 3 4 5 6
          r←assert'[1,2,[1,3,1,2,3],[1,4,4,5,6,3]]'≡#.GAP.a2ason(1 2 3)(4 5 6 3)
          r←assert'{}'≡#.GAP.a2ason ⎕NS''
          r←assert'{"a":5}'≡#.GAP.a2ason n⊣(n←⎕NS'').a←5
          r←assert'[1,2,{},{}]'≡#.GAP.a2ason ⎕NS¨'' ''
          0
      }
      test_ason2a←{
          r←assert'1'≡#.GAP.ason2a'"1"'
          r←assert'1'≡#.GAP.ason2a'[0,"1"]'
          r←assert'123456'≡#.GAP.ason2a'[1,6,"123456"]'
          r←assert(2 3⍴'123456')≡#.GAP.ason2a'[2,2,3,"123456"]'
          r←assert('123' '4563')≡#.GAP.ason2a'[1,2,[1,3,"123"],[1,4,"4563"]]'
          r←assert 1≡#.GAP.ason2a'[0,1]'
          r←assert 1 2 3 4 5 6≡#.GAP.ason2a'[1,6,1,2,3,4,5,6]'
          r←assert(2 3⍴1 2 3 4 5 6)≡#.GAP.ason2a'[2,2,3,1,2,3,4,5,6]'
          r←assert(1 2 3)(4 5 6 3)≡#.GAP.ason2a'[1,2,[1,3,1,2,3],[1,4,4,5,6,3]]'
          ns←#.GAP.ason2a'{}'
          r←assert 9.1=⎕NC⊂'ns'
          r←assert 0∊⍴ns.⎕NL-⍳9
          ns←#.GAP.ason2a'{"a":[0,5]}'
          r←assert 9.1=⎕NC⊂'ns'
          r←assert(,⊂,'a')≡ns.⎕NL-⍳9
          r←assert ns.a≡5
          nsv←#.GAP.ason2a'[1,2,{"id":2},{"id":1}]'
          r←assert 2.1=⎕NC⊂'nsv'
          r←assert 2=⍴nsv
          ns1 ns2←nsv
          r←assert∧/9.1=⎕NC'ns1' 'ns2'
          r←assert nsv.id≡2 1
          0
      }
    :EndSection

:EndNamespace
