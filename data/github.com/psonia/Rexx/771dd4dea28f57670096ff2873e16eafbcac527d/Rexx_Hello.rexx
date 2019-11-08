/* Sonia Patel */
/* infix */
/* Followed PEMDAS structure */
/* Keep in mind: When typing into the terminal, the multiplication sign is 'X', not '*' */
/* The general outline of this program: */
/* 1. Parse input from terminal and save each element in an array , prints error message */
/*    if given in incorrect format */
/* 2. Create an array of indices where 'X' is seen (Array of multiplication indices) */
/* 3. Find where the multiplication symbol is from the index array and perform  */
/*    calculations and set elements to "null" once they are used */
/* 4. Create a new array that does not contain the "null" elements and repeat */
/*    the process with /, +, and -        */


arg input

/* Function that parse and adds to array */
/* function that checks that the end hasn't been reached */

i = 0

check = 0

/* Creates array of all the numbers and operators */
do while length(input) >= 1
  parse var input num1 op remain

/* ERROR checking */
  if (num1 = '+' | num1 = '-' | num1 ='X' | num1 = '/' | num1 = '*') then
    do
      check = 1
    end
  if remain = '+' | remain = '-' | remain = 'X' | remain = '/' | remain = '*' then
    do
      check = 1
    end

/* add parsed elements to array */
  array.i = num1
  /* SAY array.i " " i */
  i = i + 1
  array.i = op
  /* SAY array.i " " i */
  i = i + 1


  input = remain

end

size = i - 1   /* save size of array */


/* checks correct format */
if size < 3 | check == 1 then
  do
    SAY "Error: This is not the correct format"
  end



else
do
/* Creates array of indices that contain 'X' symbol */
k = 0
multCount = 0

do while k < size

  if array.k = 'X' then
    do
    /*  SAY "xxxxxxxxx" k */
      opArray.multCount = k
      multCount = multCount + 1
    end

    k = k + 1
end

multIndex = multCount - 1


/* loop through array and calculate whenever 'X' is used and simplify the array */
do while multIndex >= 0
  ind = opArray.multIndex
  one = ind - 1
  two = ind + 1
  array1 = array.one
  array2 = array.two

   array.ind = "null"
   array.one = "null"
   array.two = "null"


   answer = array1 * array2

  array.one = answer

  multIndex= multIndex - 1


end


/* copy array into new array that doesn't contain "null" statements */
b = 0
divInd = 0
do while b < size
  if array.b \== "null" then
  do

  /*  SAY array.b */
    arrayDiv.divInd = array.b
    divInd = divInd + 1

  end

 b = b+1
end

/*divInd is the size arrayDiv */
size = divInd


/* Creates array of indices that contain '/' symbol */
k = 0
divCount = 0

do while k < size

  if arrayDiv.k = '/' then
    do
    /*  SAY "///////" k */
      opArrayDiv.divCount = k
      divCount = divCount + 1
    end

    k = k + 1
end

divIndex = divCount - 1


/* loop through array and calculate whenever '/' is used and simplify the array */
do while divIndex >= 0
  op = opArrayDiv.divIndex
  one = op - 1
  two = op + 1
  array1 = arrayDiv.one
  array2 = arrayDiv.two

   arrayDiv.op = "null"
   arrayDiv.one = "null"
   arrayDiv.two = "null"


   answer = array1 / array2

  arrayDiv.one = answer

  divIndex= divIndex - 1


end


/* copy array into new array that doesn't contain "null" statements */
b = 0
addInd = 0
do while b < size
  if arrayDiv.b \== "null" then
  do

  /*  SAY arrayDiv.b */
    arrayAdd.addInd = arrayDiv.b
    addInd = addInd + 1

  end

 b = b+1
end

/*divInd is the size arrayDiv */
size = addInd




/* Creates array of indices that contain '+' symbol */
k = 0
addCount = 0

do while k < size

  if arrayAdd.k = '+' then
    do
    /*  SAY "++++++" k */
      opArrayAdd.addCount = k
      addCount = addCount + 1
    end

    k = k + 1
end

addIndex = addCount - 1


/* loop through array and calculate whenever '+' is used and simplify the array */
do while addIndex >= 0
  op = opArrayAdd.addIndex
  one = op - 1
  two = op + 1
  array1 = arrayAdd.one
  array2 = arrayAdd.two

   arrayAdd.op = "null"
   arrayAdd.one = "null"
   arrayAdd.two = "null"


   answer = array1 + array2

  arrayAdd.one = answer

  addIndex= addIndex - 1


end


/* copy array into new array that doesn't contain "null" statements */
b = 0
subInd = 0
do while b < size
  if arrayAdd.b \== "null" then
  do

    /* SAY arrayAdd.b */
    arraySub.subInd = arrayAdd.b
    subInd = subInd + 1

  end

 b = b+1
end

/*divInd is the size arrayDiv */
size = subInd



/* Creates array of indices that contain '-' symbol */
k = 0
subCount = 0

do while k < size

  if arraySub.k = '-' then
    do
    /*  SAY "-------" k */
      opArraySub.subCount = k
      subCount = subCount + 1
    end

    k = k + 1
end

subIndex = subCount - 1


/* loop through array and calculate whenever '-' is used and simplify the array */
do while subIndex >= 0
  op = opArraySub.subIndex
  one = op - 1
  two = op + 1
  array1 = arraySub.one
  array2 = arraySub.two

   arraySub.op = "null"
   arraySub.one = "null"
   arraySub.two = "null"


   answer = array1 - array2

  arraySub.one = answer

  subIndex= subIndex - 1


end


/* copy array into new array that doesn't contain "null" statements */
b = 0
finalInd = 0
do while b < size
  if arraySub.b \== "null" then
  do

  /*  SAY arraySub.b */
    arrayFinal.finalInd = arraySub.b
    finalInd = finalInd + 1

  end

 b = b+1
end

/*divInd is the size arrayDiv */
size = finalInd

SAY "Your answer is" arrayFinal.0

end
