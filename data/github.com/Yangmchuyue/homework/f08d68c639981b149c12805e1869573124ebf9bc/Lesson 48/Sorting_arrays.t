var number : int
get number

var largest : string
var where : int
var word : array 1 .. number of string
var smallest : string
var sorted_list : array 1 .. number of string

for i : 1 .. number
    get word (i)
end for

largest := word (1)

for i : 2 .. number
    if word (i) > largest then
	largest := word (i)
    end if
end for


for i : 1 .. number
    smallest := word (1)
    for j : 1 .. number
	if word (j) <= smallest then
	    smallest := word (j)
	    where := j
	end if
    end for
    sorted_list (i) := smallest
    word(where) := chr(127)
end for

for i : 1 .. number
    put sorted_list (i)
end for
