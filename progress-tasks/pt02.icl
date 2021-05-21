module pt02
import StdEnv

//Given a list of integers, write a function that adds an 
//additional integer 1 after every third element.

//If the length of the list is less than 3, return itself.
add :: [Int] -> [Int]
add [] = []
add [x] = [x]
add [x,y] = [x,y]
add [x,y,z:xs]
| (length [x,y,z:xs]) >= 3 = [x,y,z] ++ [1] ++ add xs
//Start = add [1,2,3] // [1,2,3,1]
//Start = add [3..10] // [3,4,5,1,6,7,8,1,9,10]
//Start = add [1,2] // [1,2]

//Start = length [3,4,5,1,6,7,8,1,9,10]