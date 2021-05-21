module mid_prep1
import StdEnv

/**1 
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
*/
a :: [(Int, a)] -> [Int]
a [x:xs] = [fst a\\a <- [x:xs]]
//Start = a [(1,2),(2,4),(1,57)]
b :: [(Int, a)] -> [a]
b [x:xs] = [snd b\\b <- [x:xs]]
//Start = b [(1,2),(2,4),(1,57)]
Router :: [(a->b)] [(Int,a)] -> [b]
Router funcs tuples
| isEmpty funcs || isEmpty tuples = []
= [((funcs!!(x-1)) y)\\x <- (a tuples) & y <- (b tuples)] 

//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]
//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]
//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  //[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]
//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]
//Start = Router [isEven,isOdd][] //[]
/*works for all except the last. Task 1 of midterm1 sample*/

/* 1. Write a function that will return the second to last digit in a number. 
      Return 0 if there is no second digit.
*/
split :: Int -> [Int]
split 0 = []
split n = (split (abs(n)/10)) ++ [abs(n) rem 10]
//Start = split 1984
f1 :: Int -> Int
f1 n
| length (split n) < 2 = 0
| n < 0 = (split (-1 * n))!!2
= abs((split n)!!2)
//Start = f1 1234 //3
//Start = f1 5 //0
//Start = f1 -5564 //6
/*works for all except for ~(5564) */

/* 2. Write a function that will subtract numbers in a list from the first one. 
   Your solution must use 'foldr' or 'foldl'. Return 0 for an empty list.
*/
f2 :: [Int] -> Int
f2 [] = 0
f2 [x:xs] = foldl (-) x xs
//Start = f2 [10,1,2,3] //4
//Start = f2 [1,2,3,4] //-8 // 
//Start = f2 [1000,500,250,125] //125
//Start = f2 [] //0
/*works for all, Hossam's solution */

// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]
isPrime :: Int -> Bool
isPrime n
| n == 1 = True
| length ([x\\x <- [1..n] | n rem x == 0 && n <> x])== 1 = True
= False
//Start = isPrime 1
f3 :: Int -> [Int]
f3 0 = []
f3 n =  [x\\x <- [1..n]| n rem x == 0 && isPrime x]
//Start = f3 36 //[1,2,3]
//Start = f3 524287  //[1,524287]
//Start = f3 0 //[]
/*works for all */

// 7. Write a function that checks if a list of numbers is odd,even,odd,even...
// e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.
f7 :: [Int] -> Bool
f7 [] = False
f7 [x:xs]
| isEven x = False
| isOdd x = and (map isEven [a+b\\a<- [x:xs] & b <- [1..]])

//Start = f7 [1..10] //True
//Start = f7 [1,2,3] //True
//Start = f7 [2,3,4] //False
//Start = f7 [1,3,4,5] //False
//Start = f7 [1,2,3,4,6,7] //False
//Start = f7 [] //False
/*works for all */

// 8. Write a function that removes consecutive duplicates in a list.
f8 :: [Int] -> [Int]
f8 [] = []
f8 [a] = [a]
f8 [a,b:c]
| a == b = f8 (dropWhile ((==)a) c)
= [a:f8 [b:c]]
//Start = f8 [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4] //[4,5,8,4,7,0,5,4]
//Start = f8 [1,0,0,2,0,3,3,0,6,7,0,7,7] //[1,2,0,0,6,7,0]
//Start = f8 [2,0,0,6,7,5,0,8,0,5,0,0,0] //[2,6,7,5,0,8,0,5]

// 10. Write a function that checks if a number is a Mersenne Prime.
// A Mersenne Prime is a prime number that is 1 less than a power of 2. example: 7 = (2^3) - 1 = 8-1
f10 :: Int -> Bool
f10 0 = False
f10 1 = False
f10 n
| n < 0 = False
= isMember n [(2^n)-1\\n<- [1..n]]

//Start = f10 7 //True
//Start = f10 1 //False
//Start = f10 (~235) //False
//Start = f10 2147483647 //True
//Start = f10 0 //False 
/*works for all */




