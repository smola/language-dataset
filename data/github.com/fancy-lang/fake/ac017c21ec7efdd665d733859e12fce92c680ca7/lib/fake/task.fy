class Fake {
  class Task {
    include: TaskUtils

    read_write_slots: ('name, 'description, 'block, 'dependencies)

    def initialize: block {
      block call: [self]
      @name = @name to_s
      { @dependencies = [] } unless: @dependencies
      { @block = {} } unless: @block
    }

    def ran? {
      @ran? true?
    }

    def run {
      @dependencies to_a map: |d| { Fake tasks[d to_s] } . each: @{ run }

      unless: ran? do: {
        @block call_with_receiver: self
      }

      @ran? = true
    }

    def inspect {
      deps = unless: (dependencies empty?) then: { " dependencies:" + (dependencies join: ",") } else: { "" }
      "<Task:#{name} description:#{description inspect}#{deps}>"
    }
  }
}