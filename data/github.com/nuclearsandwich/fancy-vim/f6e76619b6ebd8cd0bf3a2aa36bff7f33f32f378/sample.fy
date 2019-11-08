# Test Comment Highlighting
# This file tests vim syntax highlighting. Sadly I can't actually automate
# this test so I must make a visual appraisal.

require: "test_require_keyword_highlight"

TEST_CONSTANT_HIGHLIGHTING = 1

class TestLiteralSyntaxHighlighting {
  def test_string_no_interp_highlighting {
    "test string with embedded escaped \"  double quote"
  }

  def test_string_with_interpolation {
    "Sixty-four modulo three: #{64 % 3}"
  }

  def test_heredoc_highlighting {
    """
    This is a heredocument
    """
  }

  def test_symbol_highlighting {
    'foobar
    'FO_O
    'FO%O
    'FO^O
    'FO&O
    'FO*O
    'FO-O
    'FO+O
    'FO=O
    'FO:O
    'FO|O
    'FO<O
    'FO>O
    'FO[O
    'FO]O
    'FO?O
    'FO!O
    'FO~O
    'FOO%^&*-+=:|<>[]?!~
  }

  def test_integer_highlighting {
    121
  }

  def test_negative_integer_highlighting {
    -9
  }

  def test_float_highlighting {
    1.61803

  def test_negative_float_highlighting {
    -0.000001
  }

class TestKeywordSyntaxHighlighting {
  def test_error_keywords_highlighting {
    try {
     'code
    } catch {
      'bar
      retry
    } finally {
    }
  }

  def test_return_keyword_highlighting {
    return
  }

  def test_return_local_keyword_highlighting {
    return_local
  }

  def test_match_case_stab_keyword_highlighting: value {
    match value {
      case true -> "win"
      case false -> "lose"
    }
  }

  def test_hashrocket_keyword_highlighting {
    <[ 'foo => 'bar ]>
  }

  def test_regexp_highlighting {
    /https?:(?:www)?\/\/\.google\.(com|co\.uk|hk|de)/
    /foobar(baz|qux)[-0-9]{1,2}\\\//ixmo
    /\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\Z/
  }
}

class TestVariableSyntaxHighlighting
  def test_true_and_false {
    true || false
  }

  def test_nil_highlighting {
    nil
  }

  def test_self_keyword_highlighting {
    self
  }

  def test_super_keyword_highlighting {
    super
  }

  def test_conditional_message_highlighting {
    if: true then: { 'win } else: { 'lose }
    unless: 'crime then: { 'free } else: { 'jail }
    true if_true: { "Violence" }
    false if_false: { "Peace" }
  }

  def test_loop_message_highlighting {
    done = false
    business = 'unfinished

    until: done do: {
      "workin'" println
      done = true
    }

    while: business == 'unfinished do: {
      "Finish this" println
      business = 'finished
    }

    loop: { "Non!" raise! }
  }

  def test_class_slot_highlighting {
    "Each slot should be highlighted."

    @@classapple
    @@class_apple
    @@class%apple
    @@class^apple
    @@class&apple
    @@class*apple
    @@class-apple
    @@class+apple
    @@class=apple
    @@class<apple
    @@class>apple
    @@class?apple
    @@class!apple
    @@class~apple
    @@class%^&*-+=<>?!~apple
  }

  def test_slot_highlighting {
    "Each slot should be highlighted."

    @apple
    @ap_ple
    @ap%ple
    @ap^ple
    @ap&ple
    @ap*ple
    @ap-ple
    @ap+ple
    @ap=ple
    @ap<ple
    @ap>ple
    @ap?ple
    @ap!ple
    @ap~ple
    @ap%^&*-+=<>?!~ple
  }

  def test_block_variable_highlighting {
    "|x,y| should be highlighted."

    true if_true: |x y| { x + y }
  }

  def test_dynamic_variable_highlighting {
    "The three dynamic variables should be highlighted."

    *stdout*
    *stdin*
    *stderr*
    *foo*
    *bar1_foo_233121*
  }

  def test_message_send_highlighting {
    "let:, be: and println should be highlighted one way and output another."

    let: output be: *stdout* do: {
      "I am a sorry tomato" println
    }
  }
}

