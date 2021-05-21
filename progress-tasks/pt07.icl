module pt07
import StdEnv

/*
Given a list of integers, for each element do the following:
if the element is even, check if this integer appears even amount of times in the list
if the element is odd, check if this integer appears odd amount of times in the list
*/

occur :: Int [Int] -> Int
occur _ [] = 0
occur n list = length (filter (\x= x==n) list)
//Start = occur 1 [1,2,2,1]


check :: [Int] -> [Bool]
check [] = []
check [x:xs] = [isEven a && isEven (occur a [x:xs]) || isOdd a && isOdd (occur a [x:xs]) \\a <- [x:xs] ]
//| isEven x || isOdd x = [isEven(occur x [x:xs])] ++ check xs
//= [isOdd(occur x [x:xs])] ++ check xs
//Start = check [] // []
//Start = check [1,2,2,1] // [False,True,True,False]
//Start = check [1,1,1,2,2,2,3,5,3,3,5] // [True,True,True,False,False,False,True,False,True,True,False]