note
	description: "[
		Separate the string "Hello,How,Are,You,Today" by commas into an array (or list) so that each element of it 
		stores a different word. Display the words to the 'user', in the simplest manner possible, separated by a period.
		To simplify, you may display a trailing period.
		]"
	EIS: "name=Tokenize String Requirement", "protocol=URI", "src=http://rosettacode.org/wiki/Tokenize_a_string", "tag=requirement"

class
	TOKENIZE_STRING_EXAMPLE

create
	make

feature
	make
			-- Run example.
		local
			str: STRING -- String to tokenize
			l_words: LIST[STRING] -- List of words
		do
			str := "Hello,How,Are,You,Today"
			print ("%NString to tokenize:"+ str)

			l_words := str.split (',')
			print ("%NWords separeted by period:" + display_words (l_words))
		end


	display_words (a_list: LIST[STRING] ): STRING
			-- Display the words of the list `a_list' separeted by a period
		do
			Result := ""
			across a_list as elem loop
			    Result.append (elem.item)
			    Result.append_character ('.')
			end
			Result.remove_tail (1)
		end

end
