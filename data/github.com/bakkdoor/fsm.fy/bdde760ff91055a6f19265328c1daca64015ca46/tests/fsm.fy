FancySpec describe: FSM with: {
  it: "defines a FSM with states & transitions" when: {
    find_a = FSM new: {
      start: @{
        + "a" -> end   # end state, since it has no transitions
        + _   -> start
      }
    }

    find_a states size is: 2
    find_a states keys is: ["start", "end"]
    find_a states values map: @{ transitions } flatten size is: 2

    find_a <- "a" . is: true
    find_a <- "b" . is: false
    find_a <- "bbbbb" . is: false
    find_a <- "bbbbba" . is: true
  }

  it: "parses numbers" with: '<- when: {
    parse_num = FSM new: {
      digit: @{
        + /[0-9]/ -> digit
        + " "     -> digit
        + "."     -> end
      }
    }

    ["abc", "123a", "123 456 a", "123 456 .a", "123.123"] each: |input| {
      { parse_num <- input } raises: FSM ProcessFailure
    }

    parse_num <- "123." . is: true
    parse_num <- "123." . is: true
    parse_num <- "123 123" . is: false
    parse_num <- "123 123." . is: true
  }

  it: "processes input from any Fancy::Enumerable" with: '<- when: {
    parse_nums = FSM new: {
      parse_num: @{
        + (0..9) -> parse_num
        + 'done  -> end
      }
    }

    parse_nums <- [0,1,2,3,4,5,6,7,8,9] . is: false
    parse_nums <- [0,1,2,3,4,5,6,7,8,9,'done] . is: true

    {
      parse_nums <- [0,1,2,3,4,5,6,7,8,9,10]
    } raises: FSM ProcessFailure
  }

  it: "calls a callback with the matched input" when: {
    even = []
    collect_even = FSM new: {
      final numbers: @{
        + @{ even? } -> numbers do: |num| { even << num }
        + _          -> numbers
      }
    }

    collect_even <- (0..10) . is: true
    even is: [0,2,4,6,8,10]
  }

  it: "defines loop & multiple final states" when: {
    multiple_final_states = FSM new: {
      start: @{
        + /[a-z]/ -> letters
        + /[0-9]/ -> numbers
        + _       -> end
      }

      # final, even though it has transitions
      final letters: @{
        + /[a-z]/ -> letters
        + _       -> fail
      }

      final numbers: @{
        + /[0-9]/ -> numbers
        + _       -> fail
      }

      loop fail
    }

    multiple_final_states <- "abc" . is: true
    multiple_final_states <- "abc1" . is: false
    multiple_final_states <- "123" . is: true
    multiple_final_states <- "123a" . is: false
  }

  it: "performs transitions conditionally" when: {
    even_digits = []
    odd_digits  = []

    fsm = FSM new: {
      start: @{
        + /[0-9]/ ?? @{
          ? @{ to_i even? } -> even ! |x| { even_digits << x }
          ? @{ to_i odd? }  -> odd  ! |x| { odd_digits << x }
        }
      }

      final even: @{
        + @{ to_i even? } -> even ! |x| { even_digits << x }
      }

      final odd: @{
        + @{ to_i odd? }  -> odd  ! |x| { odd_digits << x }
      }
    }

    fsm <- "02468" . is: true
    even_digits is: $ "02468" split: ""

    even_digits = []
    odd_digits  = []
    { fsm <- "012468" } raises: FSM ProcessFailure
    even_digits is: ["0"]
    odd_digits is: []

    even_digits = []
    odd_digits  = []
    fsm <- "13579" . is: true
    even_digits is: []
    odd_digits is: $ "13579" split: ""

    even_digits = []
    odd_digits  = []
    { fsm <- "123579" } raises: FSM ProcessFailure
    even_digits is: []
    odd_digits is: ["1"]
  }
}