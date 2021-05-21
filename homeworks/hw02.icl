module hw02
import StdEnv

// 1. Give a list of numbers, multiplying all 
// even numbers by 2 and all odd numbers by 3
multiply :: [Int] -> [Int]
multiply [] = []
multiply [x:xs]
| isEven x = [x*2] ++ multiply xs
= [x*3] ++ multiply xs
//multiply list = [x*2\\ x<- list | isEven x] ++ [x*3\\x<-list | isOdd x]

//Start = multiply [14, 22, 45, 56] // [28, 44, 135, 112]
//Start = multiply [13, 27, 44] // [39, 81, 88]
//Start = multiply [] // []
//works for all cases


// 2. Given a list of integers, find the prime numbers and compute the sum of them.
// Return 0 for empty lists or if there are no primes.
is_prime :: Int -> Bool
is_prime 0 = False
is_prime 1 = False
is_prime n
|n > 0 && isEmpty [x\\x<-[2..n] | n rem x == 0 && n <> x ] = True
= False
//Start = is_prime -4 //False
//Start = [x\\x<-[2..1000] | is_prime x]

sum_of_prime :: [Int] -> Int
sum_of_prime [] = 0
sum_of_prime list = sum[x\\x <-list | is_prime x]

//Start = sum_of_prime [14, 22, 45, 56] // 0
//Start = sum_of_prime [13, 27] // [13]
//Start = sum_of_prime [13, 3, 76, 17] // 33
//Start = sum_of_prime [] // 0
//works for all cases


/*
3. Given two lists of integers of the same length, 
check if the elements in two lists with the same 
index are of the same property (both even or both odd).
Return True for empty lists
*/
same :: [Int] [Int] -> Bool
same [] [] = True
//same [] _ = False
//same _ [] = False
same [x:xs][y:ys]
| and[isEven(x+y)\\ x<-[x:xs]& y<-[y:ys]]= True
= False

//Start = same [1,2,3] [2,4,6] // False
//Start = same [1,2,3,4] [3,8,5,12] // True
//Start = same [] [] // True
//Start = same [5,4,3,3] [9,4,5,5] //True
//works for all cases

//another version
s :: [Int] [Int] -> Bool
s [] [] = True
s [] _ = False
s _ [] = False
s [x:xs][y:ys] = and[isEven x && isEven y || isOdd x && isOdd y\\x<-[x:xs]& y<-[y:ys]]
//Start = s [1,2,3] [2,4,6] //False
//Start = s [1,2,3,4] [3,8,5,12] // True
//Start = s [] [] // True
//Start = s [5,4,3,3] [] //False
//Start = s[5,4,3,3] [9,4,5,6] //False












//does not belong to the homework solution
/*
s1 :: [Int] [Int] -> Bool
s1 [] [] = True
s1 [] _ = False
s1 _ [] = False
s1 [x:xs][y:ys]
|isEven x && isEven y && s1 xs ys = True
|isOdd x && isOdd y && s1 xs ys = True
*/
//Start = s1 [1,2,3] [2,4,6] //False
//Start = s1 [1,2,3,4] [3,8,5,12] // True
//Start = s1 [] [] // True
//Start = s1 [5,4,3,3] [] //False
//Start = s1 [5,4,3,3] [9,4,5,6] //False





