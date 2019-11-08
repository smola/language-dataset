class Gossip {
  """
  Gossip prints out a interview between two actors.
  """
  def initialize: @block {
    @count = 1
  }

  def reply: other notify: parent {
    @block call: [@count]
    @count = @count + 1
    if: (@count > 50) then: {
      parent run # resume execution
    } else: {
      other @@ reply: self notify: parent
    }
  }
}

bar = Gossip new: |count| { "Question: #{count}" println }
foo = Gossip new: |count| { "Answer  : #{count}" println }

foo @@ reply: bar notify: $ Thread current

"Start gossip" println
Thread stop # Pause execution
"Gossip done" println
