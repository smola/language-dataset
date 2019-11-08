module SampleMidterm2
import StdEnv

// 1. Write a function that will return the second to last digit in a number. Return 0 if there is no second digit.

//f1 :: Int -> Int

//Start = f1 1234 //3

//Start = f1 5 //0

//Start = f1 ~(5564) //6


// 2. Write a function that will subtract numbers in a list from the first one. Your solution must use 'foldr' or 'foldl'.

// Return 0 for an empty list.

//f2 :: [Int] -> Int

//Start = f2 [10,1,2,3] //4

//Start = f2 [1,2,3,4] //-8

//Start = f2 [1000,500,250,125] //125

//Start = f2 [] //0


// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]

//f3 :: Int -> [Int]

//Start = f3 36 //[1,2,3]

//Start = f3 524287  //[1,524287]

//Start = f3 0 //[]


// 4. Write a function that reverses tuples from a list if the tuple members sum up to an even number.

//f4 :: [(Int, Int)] -> [(Int, Int)]

//Start = f4 [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]

//Start = f4 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]

//Start = f4 [] //[]


// 5. Write a function that takes every number in a list and generates a sublist of its first 5 multiples. Your solution must use 'map'.

//f5 :: [Int] -> [[Int]]

//Start = f5 [1..3] //[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15]]

//Start = f5 [4,~3,5,~6] //[[4,8,12,16,20],[-3,-6,-9,-12,-15],[5,10,15,20,25],[-6,-12,-18,-24,-30]]

//Start = f5 [] //[]


// 6. Given an integer n, find the minimal k such that

// k = m! (where m! = 1 * 2 * ... * m) for some integer m; k >= n.

// In other words, find the smallest factorial which is not less than n.

// example: leastfactorial 17 = 24.  because 17 < 24 = 4! = 1 * 2 * 3 * 4, while 3! = 1 * 2 * 3 = 6 < 17

//leastfactorial :: Int -> Int

//Start = leastfactorial 17 // 24

//Start = leastfactorial 1 // 1

//Start = leastfactorial 5 // 6

//Start = leastfactorial 25 // 120


// 7. Write a function that checks if a list of numbers is odd,even,odd,even...

// e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.

//f7 :: [Int] -> Bool

//Start = f7 [1..10] //True

//Start = f7 [1,2,3] //True

//Start = f7 [2,3,4] //False

//Start = f7 [1,3,4,5] //False

//Start = f7 [1,2,3,4,6,7] //False

//Start = f7 [] //False


// 8. Write a function that removes consecutive duplicates in a list.

//f8 :: [Int] -> [Int] 

//Start = f8 [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4] //[4,5,8,4,7,0,5,4]

//Start = f8 [1,0,0,2,0,3,3,0,6,7,0,7,7] //[1,2,0,0,6,7,0]

//Start = f8 [2,0,0,6,7,5,0,8,0,5,0,0,0] //[2,6,7,5,0,8,0,5] 


// 9. Write a function that takes a tuple of three lists and generates a list of triple tuples.

// The triple tuple is only generated if the i-th member of the first list multiplied by the

// i-th member of the second list equals the i-th member of the third list.

// e.g. for ([1,2,3,4,5],[2,4,6,8,10],[2,8,17,32,45]) the result is [(1,2,2),(2,4,8),(4,8,32)]

//f9 ::([Int],[Int],[Int])->[(Int,Int,Int)]

//Start = f9 ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10])//[(2,1,2),(2,2,4),(2,3,6),(2,5,10)]

//Start = f9 ([1,2,3,4,5],[2,4,6,8,10],[2,8,1,32,45])//[(1,2,2),(2,4,8),(4,8,32)]

//Start = f9 ([1,0,1,0,1,0],[3,4,5,6,8],[3,0,5,0,0])//[(1,3,3),(0,4,0),(1,5,5),(0,6,0)]


// 10. Write a function that checks if a number is a Mersenne Prime.

// A Mersenne Prime is a prime number that is 1 less than a power of 2. example: 7 = (2^3) - 1 = 8-1

//f10 :: Int -> Bool

//Start = f10 7 //True

//Start = f10 1 //False

//Start = f10 (~235) //False

//Start = f10 2147483647 //True

//Start = f10 0 //False