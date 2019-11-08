class Expected
  def initialize(object:int)
    @expected = object
    @not_assertion = false
  end

  def not
    @not_assertion = true
    self
  end

  defmacro define_matcher(name, &block) do
    quote do
      def `name`(object:int)
        @actual = object
        assertion = `block.body`
        @not_assertion ? !assertion : assertion
      end
    end
  end

  define_matcher 'to_equal' do
    @expected == @actual
  end

  define_matcher 'to_be_greater_than' do
    @expected > @actual
  end
end

class ExpectTest
  def expect(expected:int)
    Expected.new(expected)
  end

  def run_test(name:string)
    puts name
    # need to work out how to invoke the test..
  end
end

class MatchersTest < ExpectTest
  def test_int_equivalence
    puts expect(123).to_equal(123)
    puts expect(123).not.to_equal(456)
  end

  def test_int_greater_than
    puts expect(2).to_be_greater_than(1)
    puts expect(1).not.to_be_greater_than(2)
  end
end

test_class = MatchersTest.new
test_class.test_int_equivalence
test_class.test_int_greater_than

methods = test_class.getClass.getDeclaredMethods
methods.each do |method|
  name = method.getName
  if name.startsWith("test_")
    test_class.run_test(name)
  end
end

