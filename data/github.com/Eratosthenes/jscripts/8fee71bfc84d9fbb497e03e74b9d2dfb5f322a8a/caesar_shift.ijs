#!/Applications/j803/bin/jc
NB. usage: jc caesar_shift.ijs or ./caesar_shift.ijs (probably need to make file executable first)
NB. does a caesar shift w/o changing spaces
NB. to make jc a command, had to do ln -s /Applications/j803/bin/jc /usr/bin/local
NB. echo and exit are valid J commands, not bash
data =. 'hello cruel world'
shift =. 2

ascii =. a. i. data NB. convert data to ascii
space_idx =. I. 32 = ascii NB. find indices of spaces
result =. u: 32 (space_idx)} shift + ascii NB. shift data, replace the spaces and convert back to letters

echo data
echo result
exit ''
