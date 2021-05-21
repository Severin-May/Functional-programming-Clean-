module pt01
import StdEnv

/*
fact :: Int -> Int
fact 0 = 1
fact n
| n < 0 = -1
= n * fact(n-1)
*/
//Start = fact 5 // 120
//Start = fact 1 // 1
//Start = fact (-10) // -1

lala :: [Int] -> Int
lala [] = 0
lala [x,y:xs] = y + lala xs
//Start = lala [1..10]
//Start = sum[z\\z<-[1..10]| isEven z]