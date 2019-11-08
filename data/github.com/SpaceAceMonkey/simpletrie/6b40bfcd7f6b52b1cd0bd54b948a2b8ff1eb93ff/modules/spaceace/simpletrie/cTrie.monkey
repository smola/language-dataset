Strict

Import cTrieNode
Import cSymbolFrequencyPair
Import cSymbolFrequencyList

#rem monkeydoc
Functions to build and manipulate a trie structure. All functions are statis, and do
not require you to instantiate the Trie class.
#END
Class Trie
	#rem monkeydoc
		The alphabet to use for the data you will be storing in this trie.
	#END
	Global ALPHABET:String[] =["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
	#rem monkeydoc
		This variable is incremented each time a new leaf node is discovered.
		This is effectively a count of the number of "words" in the trie.
	#END
	Global LEAF_NODE_COUNT:Int = 0
	#rem monkeydoc
		Used as an empty/false return value
	#END
	Global NIL:TrieNode
	
	#rem monkeydoc
		Use this to change the alphabet this trie will be using. If you want 
		to use a custom alphabet, call this before doing any insertions or 
		lookups.
	#END
	Function SetAlphabet:Void(alphabet:String[])
		ALPHABET = alphabet[ ..]
	End Function
	
	#rem monkeydoc
		Insert a word into the trie, starting at root. Generally, this will be the
		very first node of the entire trie, but that is not a requirement.
	#END
	Function Insert:Void(root:TrieNode, key:String)
		Local index:Int
		Local node:TrieNode = root
		
		For Local letter:String = EachIn(key.Split(""))
			index = IndexFromCharacter(letter)
			If Not (node.children[index])
				node.children[index] = New TrieNode(ALPHABET.Length)
			EndIf
			node = node.children[index]
		Next

		node.isLeaf = True
		LEAF_NODE_COUNT += 1
	End Function
	
	#rem monkeydoc
		Check the trie for a given value. Return true if the value is found; false, otherwise.
	#END
	Function Contains:Bool(root:TrieNode, key:String)
		Local node:TrieNode = GetNode(root, key, False)
		
		Return (node And node.isLeaf)
	End Function
	
	#rem monkeydoc
		Descend into the trie, looking for the node that corresponds to 
		key. This function will return either a node, or NIL.
		
		If returnPartial is true, GetNode() will return the last node it
		found, whether or not the entire key was matched. You cannot
		trust the isLeaf member of a node returned with returnPartial
		set to true. Here is an illustration of why:
		
		Key: acerbicz
		Trie: a -> c -> e -> r -> b -> i -> c [leaf node] -> z
		
		"Acerbic" is a word, but there is no word that starts with 
		"acerbicz." If returnPartial is true, GetNode() will find its
		way to the "c" in acrbic, then it will not be able to advance,
		since there will not be a z node attached to the c node. In this
		case, GetNode() will return the c node.
	#END
	Function GetNode:TrieNode(root:TrieNode, key:String, returnPartial:Bool = True)
		Local index:Int
		Local node:TrieNode = root
		
		For Local letter:String = EachIn(key.Split(""))
			index = IndexFromCharacter(letter)
			If Not (node.children[index])
				If Not (returnPartial)
					Return NIL
				EndIf
				
				Return node
			EndIf
			
			node = node.children[index]
		Next
		
		Return node
	End Function
	
	
	#rem
		Build and store every complete value that exists in the trie from
		the node specified in root on down. "Complete" values are strings
		of nodes that end in a leaf. Therefore, a trie structure of
		a -> n -> d [leaf] -> r -> o -> i -> d [leaf]
		will return the values "and" and "android."
			
		You must provide a StringStack to hold the resulting lexicon.
	#END
	Function GetLexicon:Void(root:TrieNode, lexicon:StringStack, currentWord:String = "")
		For Local i:Int = 0 To root.children.Length - 1
			If (root.children[i])
				currentWord += ALPHABET[i]
				Local node:TrieNode = root.children[i]
				GetLexicon(node, lexicon, currentWord)
				currentWord = currentWord[ .. currentWord.Length - 1]
			EndIf
		Next

		If (root.isLeaf)
			lexicon.Push(currentWord)
		EndIf
	End Function

	#rem
		Find a symbol's index in your alphabet.
	#END
	Function IndexFromCharacter:Int(character:String)
		For Local i:Int = 0 To ALPHABET.Length - 1
			If (ALPHABET[i] = character)
				Return i
			End If
		Next
		
		Return -1
	End Function
	
	#rem monkeydoc
		More of a curiosity than a necessary tool, this will 
		tell you the frequency of each symbol in your trie,
		starting at a given node.
	#END
	Function GetSymbolFrequency:SymbolFrequencyList(root:TrieNode)
			Local wordList:StringStack = New StringStack()
			GetLexicon(root, wordList)
			Local frequencyArray:SymbolFrequencyPair[ALPHABET.Length]
			For Local word:String = EachIn(wordList)
				For Local letter:String = EachIn(word.Split(""))
					Local index:Int = IndexFromCharacter(letter)
					If Not (frequencyArray[index])
						frequencyArray[index] = New SymbolFrequencyPair(letter, 1)
					Else
						frequencyArray[index].occurrences += 1
					End If
				Next
			Next

			Return New SymbolFrequencyList(frequencyArray)
	End Function
End Class

