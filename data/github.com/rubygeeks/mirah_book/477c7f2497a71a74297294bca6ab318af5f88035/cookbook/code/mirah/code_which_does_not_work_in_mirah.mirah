

#syntax/methods different in java, need to use that one

# from strings/1_building_strings_from_parts.mirah


#hash and array is implemented differently would need to know native java ways
#hash = { "key1" => "val1", "key2" => "val2" }
#string = ""
#hash.each { |k,v| string << "#{k} is #{v}\n" } 
#puts string
# key1 is val1
# key2 is val2
#---
#string = ""
#hash.each { |k,v| string << k << " is " << v << "\n" } 
#---
#puts hash.keys.join("\n") + "\n"
# key1
# key2
#---
#data = ['1', '2', '3']
#puts data
#puts data.methods
#s = ''
#data.each { |x| s << x << ' and a '}
#puts s                                             # => "1 and a 2 and a 3 and a "

#puts data.join(' and a ')                          # => "1 and a 2 and a 3"
#---
#s = ""
#data.each_with_index { |x, i| s << x; s << "|" if i < data.length-1 }
#s                                             # => "1|2|3"
#---




#code from strings/02_substituting_variables_into_strings.mirah


# mirah does not have next method for number class
# puts "The number after #{number} is #{number.next}." 
# => "The number after 5 is 6."


# not supported in mirah
#%{Here is #{class InstantClass
#   def bar 
#      "some text" 
#    end 
#  end 
#  InstantClass.new.bar
#}.}
## =>