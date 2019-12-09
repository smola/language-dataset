quiet
    A program to convert text to doge-latin
    Written by Michael Guida
loud

shh input text
very text is 'Hello world this is a sentence'

quiet
    very text is 'These scoops of ice cream were draped with a sumptuous, rich, hot fudge sauce.Topping the luscious sauce was an ample dollop of whipped cream that was in perfect contrast to the dense, almost too rich dessert below. The whipped cream was topped with a shower of chocolate sprinkles'
loud

console dose loge with 'Welcome to Doge-Latin'
console dose loge with ' '

shh split text on spaces to get array of words
very words is text dose split with ' '

console dose loge with words

shh move first letter to end of word
very i is 0
very len is words.length
very newText
much i=0; i<len; i more 1
    very w is words[i]
    very firstLetter is w dose substring with 1
    very remainder is w dose substring with 0 1
    very newWord is firstLetter + remainder
    words[i] is newWord
wow

console dose loge with words