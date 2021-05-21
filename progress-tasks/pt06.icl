module pt06
import StdEnv

//Given two lists A and B. Return the list which contains 
//their intersection. We define intersection as a list, 
//where i-th character of A is included if A_i != B_i. 
// Ex. [1,2,3,4] [1,4,3,5] -> [2,4]

intersection :: [Int] [Int] -> [Int]
intersection _ [] = []
intersection [] _ = []
intersection a b = [x\\x <- a & y <- b | x <> y ]
//Start = intersection [1,2,4,4] [1,4,3,5] // [2,4,4]
//Start = intersection [1..5] [1..5] // []
//Start = intersection [1,1,2,3,4,6] [1,2,4,2,3,6] // [1,2,3,4]
//Start = intersection [] [1..4] // []
//Start = intersection [1..10] [2..7] // [1,2,3,4,5,6]
// 1 2 3 4 5 6 7 8 9 10
// 2 3 4 5 6 7 

inter :: [Int] [Int] -> [Int]
inter _ [] = []
inter [] _ = []
inter [x:xs] [y:ys]
| x <> y = [x] ++ (inter xs ys)
= inter xs ys
//Start = inter [1,2,4,4] [1,4,3,5] // [2,4,4]
//Start = inter [1..5] [1..5] // []
//Start = inter [1,1,2,3,4,6] [1,2,4,2,3,6] // [1,2,3,4]
//Start = inter [] [1..4] // []
//Start = inter[1..10] [2..7] // [1,2,3,4,5,6]









