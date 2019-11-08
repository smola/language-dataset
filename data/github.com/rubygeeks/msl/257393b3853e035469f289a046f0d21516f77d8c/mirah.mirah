package mirah

import java.lang.Integer
import java.lang.Float


class MString

def initialize(s:string)
@s=s

end


def to_s
@s
end

def to_i
Integer.parseInt( @s);
end

def to_f
Float.parseFloat(@s)
end

def methods
["to_i","to_s","to_f","methods","help"]
end

def help

" #Usage
import mirah.MString
sint = MString.new(\"20\")
sfloat=MString.new(\"20.5\")
puts sint.to_i
puts sfloat.to_f
puts sint.methods
puts sint.help
"
end

end


class MArray

def initialize

end

def length

end

end


class MTime

def initialize

end

def self.now

end

end