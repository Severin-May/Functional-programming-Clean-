module mid_prep
import StdEnv

// 1. Write a function that will return the second to last digit in a number. 
// Return 0 if there is no second digit.

f1 :: Int -> Int
f1 n = (n/10)rem 10
//Start = f1 12344 //3
//Start = f1 5 //0
//Start = f1 ~(5564) //6

// 2. Write a function that will subtract numbers in a list 
// from the first one. Your solution must use 'foldr' or 'foldl'.
// Return 0 for an empty list.

f2 :: [Int] -> Int
f2 [] = 0
f2 list = foldl (-) 0 (sort list)
//Start = f2 [10,1,2,3] //4
//Start = f2 [1,2,3,4] //-8
//Start = f2 [1000,500,250,125] //125
//Start = f2 [] //0

