module midterm
import StdEnv

/*---------------------------------------------------------------
-- Functional Programming & mid-term, 2021. Apr. 9.
-- This solution was submitted and prepared by <Aichurok, RIMR52> for the mid-term assignment of the Functional Programming course.
-- I declare that this solution is my own work.
-- I have not copied or used third-party solutions.
-- I have not passed my solution to my classmates, neither made it public.
-- Students' regulation of Eotvos Lorand University (ELTE Regulations Vol. II. 74/C.) states that as long as
-- a student presents another student's work - or at least the significant part of it - as his/her own performance,
-- it will count as a disciplinary fault.
-- The most serious consequence of a disciplinary fault can be dismissal of the student from the University.
*/

// 1. Given a list of lists of tuples containing integers.
// Substitute each sublist with the number of tuples in that sublist having the property that 
// the first element is greater than the second one.
// Example:
// [[(1,2),(2,2),(3,2)],[],[(5,2),(3,4)]] -> [1,0,1]
// [(1,2),(2,2),(3,2)] -> 1 (3,2)
// [] -> 0
// [(5,2),(3,4)] -> 1 (5,2)

isGreat :: (Int, Int) -> Bool
isGreat (a,b) 
| a > b = True
= False
//Start = isGreat (2,2)

aux1 :: [(Int, Int)] -> Int
aux1 [] = 0
aux1 [x:xs] = sum[1\\ n <- [x:xs] | isGreat n]
//Start = aux1 [(1,2),(2,2),(3,2)]

tuples_with_property :: [[(Int,Int)]] -> [Int]
tuples_with_property [] = []
tuples_with_property list = map aux1 list 

//Start = tuples_with_property [[(1,2), (2,2), (3,2)], [], [(5,2), (3,4)]] // [1,0,1]
//Start = tuples_with_property [[(5,3), (2,1122), (3123,21)], [(1,-123), (0,0)], [(5,2), (3222,4)], [(5,2), (3,4)]] // [2,1,2,1]
//Start = tuples_with_property [[(1,2), (2,2), (1,2)], [(2,2), (3,4)]] // [0,0]
/*works for all */

// 2. Given a positive integer number. Create a list of lists like:
// [[n,n-1,..1,1,..,n],[n-1,n-2,..1,1,..,n-1]....[2,1,1,2],[1,1],[1,1],[2,1,1,2],.....[n-1,n-2,..1,1,..,n-1],[n,n-1,..1,1,..,n]]
// Example: for 3 the created list is:
// [[3,2,1,1,2,3],[2,1,1,2],[1,1],[1,1],[2,1,1,2],[3,2,1,1,2,3]]
 
decr :: Int -> [Int]
decr n
| n <= 0 = []
= [n] ++ decr (n-1)
//Start = decr 3

decreasing :: Int -> [[Int]]
decreasing n
| n <= 0 = []
= [(decr n) ++ reverse (decr n)] ++ decreasing (n-1) 


decreasing_list :: Int -> [[Int]]
decreasing_list n
| n <= 0 = []
= decreasing n ++ reverse (decreasing n)

//Start = decreasing_list 3 // [[3,2,1,1,2,3],[2,1,1,2],[1,1],[1,1],[2,1,1,2],[3,2,1,1,2,3]]
//Start = decreasing_list 5 // [[5,4,3,2,1,1,2,3,4,5],[4,3,2,1,1,2,3,4],[3,2,1,1,2,3],[2,1,1,2],[1,1],[1,1],[2,1,1,2],[3,2,1,1,2,3],[4,3,2,1,1,2,3,4],[5,4,3,2,1,1,2,3,4,5]]
//Start = decreasing_list 1 // [[1,1],[1,1]]
/* works for all*/

// 3. Write a function that takes a list of lists
// and a function. This function returns True if
// the length of each list and its position in the list of lists
// have the same output of the function
// example: [[1,2,1], [1,4]] isEven
// returns True because length of [1,2,1] is 3 and its position
// is 1 so isEven gives False for both of them, and same for [1,4]
// Note: position starts from 1 in this task.


match :: [a] (Int->b) -> Bool | == b
match [] _ = False
match list func
| func (length list) = True
= False

matchIndex :: [[a]] (Int -> b) -> Bool | == b
matchIndex [] _ = False
matchIndex [x:xs] func
| (match x func) && func ([x:xs]!!0) && (matchIndex xs func) = True
= False

//Start = matchIndex [[1,2,1], [1,4]] isEven // True
// Start = matchIndex [[1,2,1], [1,4]] (\x = x+1) // False
// Start = matchIndex [["a"], ["a", "b"]] ((+)5) //True
// Start = matchIndex [[1.0,6.0,11.0]] isOdd // True

// 4. Write a function which takes a list of lists
// and returns the biggest even sum of the sublists' sums
// example: [[1,2,3],[],[10,1],[2,2]] -> 6 because the sum
// of [1,2,3] is 6 and it is biggest even between the other sums
// 0, 11 and 4
// Note: sum of empty list is 0
// Note: if there is no even sums, return -1
 

hasEven :: [Int] -> Bool
hasEven [] = False
hasEven list
| isEmpty [x\\x<- list | isEven x] = False
= True
//Start = hasEven [2,7]

max_even_sum :: [[Int]] -> Int
max_even_sum [] = 0
max_even_sum list
| hasEven [sum x\\x<- list]  = maxList [a\\a<- [sum x\\x<- list] | isEven a]
= -1

//Start = max_even_sum [[1,2,3],[],[10,1],[2,2]] //6
//Start = max_even_sum [[1,1], [9,9], [100,1]] //18
//Start = max_even_sum [[],[1]] //0
//Start = max_even_sum [[1],[3,4]] //-1
/*works for all */

// 5. Given a list of lists of integers, for each list,
// extract the prime number and the palindrome number.
// (There is only one prime and only one palindrome in each sublist).

isPrime :: Int -> Bool
isPrime n
| n < 2 = False
| isEmpty [x\\x <- [2..toInt(sqrt(toReal n))] | n rem x == 0] = True
= False

split :: Int -> [Int]
split 0 = []
split n = split (n/10) ++ [n rem 10]
//Start = reverse (split -1984)
 
is_palindrome :: Int -> Bool
is_palindrome n
| (split n) == (reverse (split n)) = True
= False

extract :: [[Int]] -> [(Int, Int)]
extract [] = []
extract [x:xs] = [(hd[n\\n <- x | isPrime n], hd[n\\n <- x | is_palindrome n])] ++ extract xs

//Start = extract [] //[]
//Start = extract [[10,121,20,23], [71,62,81,999], [42,16,33,13], [4,23,91]] // [(23,121), (71,999), (13,33), (23,4)]
//Start = extract [[22,19,42], [21,47,909]] // [(19,22),(47,909)]
/* works for all */

// 6. Given an integer, return a list that has all the prime factors of the given number.
// The list should contain the prime factors in ascending order.
// Note: we do not consider 1 a prime number.

priaux:: Int -> [Int]
priaux n
| n < 2 = []
= [a\\a<- [2..n] | n rem a == 0 && isPrime a]
//Start = priaux 4

primeFactors :: Int -> [Int]
primeFactors 0 = []
primeFactors 1 = []
primeFactors n = [x\\x <- [a\\a<- [2..n] | n rem a == 0] | isPrime x]

//Start = primeFactors 0 // []
//Start = primeFactors 1 // []
//Start = primeFactors 17 // [17]
//Start = primeFactors 374 // [2, 11, 17]
//Start = primeFactors 672 // [ 2, 3,  7]
//Start = primeFactors 41533164779// [19, 23,31, 37, 41, 43, 47]

// 7. Calculate Euler's totient function phi(m).
// Euler's so-called totient function phi(m) is defined as the number of 
// positive integers r (1 <= r < m) that are coprime with m.
// Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
// Two integers a and b are coprime, if the only positive integer that evenly divides 
// (is a divisor of) both of them is 1.
 
divisor :: Int -> [Int]
divisor n
| n < 2 = []
= [a\\ a <- [1..n] |n rem a == 0] 
//Start = divisor 3

isCoprime :: Int Int -> Bool
isCoprime 1 b = True
isCoprime a 1 = True
isCoprime a b
| length([x\\x<-(divisor a)| isMember x (divisor b)]) == 1 = True
= False
//Start = isCoprime 10 2

phi :: Int -> Int
phi 1 = 1
phi n = sum[1\\x<-[1..n]|(isCoprime x n)]

//Start = phi 1 // 1
//Start = phi 10 // 4
//Start = phi 12414 // 4136
//Start = phi 100 // 40
//Start = phi 1000000 // 400000


// 8. Determine the prime factors of a given positive integer.
// Construct a list containing the prime factors and their multiplicities.
 
divisors :: Int -> [Int]
divisors n
| n < 2 = []
= [a\\ a <- [2..n] |n rem a == 0 && isPrime a] 
//Start = divisors 7

/*
multiplicity n x
|n rem x == 0 = 1 + (multiplicity (n/x) x)
= 0

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = [(x, (multiplicity n x)) \\ x <- [1..n] | isPrime (x) && not (multiplicity n x == 0)]
*/

occur :: Int [Int] -> Int
occur _ [] = 0
occur n list = length (filter (\x= x==n) list)
//Start = occur 1 [1,2,2,1]

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n
| n <= 0 = []
= [a\\ a <- [(x, occur x (divisors n))\\x <- (divisors n)]]
 
//Start = prime_factors_mult 4 // [(2,2)]
//Start = prime_factors_mult 315 // [(3,2), (5,1), (7,1)]
//Start = prime_factors_mult 204 // [(2,2), (3,1), (17,1)]
//Start = prime_factors_mult 230 // [(2,1), (5,1), (23,1)]
//Start = prime_factors_mult 251 // [(251,1)]
//Start = prime_factors_mult 676 // [(2,2), (13,2)]
/* check it */

// 9. You are given a list of segments and an interval.
// Each segment and interval is described with a tuple, which contains their left and right endpoints.
// Return segments from the list which intersect with the given interval.
// For example: (1, 4) intersects with (2, 7), but (1, 3) does not have an intersection with (5, 10).
// Ex. [(1,3), (2,5), (6,7), (4,7), (1,2)] (3, 5) -> [(1,3), (2,5), (4,7)]

inter :: (Int, Int) (Int, Int) -> Bool
inter (a,b) (x,y)
| a >= x && a <= y || b >= x = True
= False
//Start = inter (1,2)(3,5)

intersection :: [(Int,Int)] (Int,Int) -> [(Int,Int)]
intersection [] _ = []
intersection tupls seg = [x\\x <- tupls | inter x seg]

//Start = intersection [(1,3), (2,5), (6,7), (4,7), (1,2)] (3, 5) // [(1,3), (2,5), (4,7)]
//Start = intersection [(2,3), (2,5), (2,7), (5,7), (1,2)] (3, 5) // [(2,3),(2,5),(2,7),(5,7)]
//Start = intersection [] (3, 5) // []
//Start = intersection [(2,3), (2,5), (2,7), (5,7), (1,2)] (10, 15) // []
//works for all

// 10. Given the list of tuples, where each tuple contains 3 integers. Write a function which
// sorts all numbers in ascending order, but keeps the sorted numbers in tuples of 3.
// Ex.: [(3,2,7),(1,6,8),(5,9,4)] -> [(1,2,3),(4,5,6),(7,8,9)]

tlist :: [(Int,Int,Int)] -> [Int]
tlist [] = []
tlist [x:xs] = sort([(fst3 x)] ++ [(snd3 x)] ++ [(thd3 x)] ++ tlist xs)
//Start = tlist [(3,2,7),(1,6,8),(5,9,4)]

toTuple :: [Int] -> [(Int, Int, Int)]
toTuple [] = []
toTuple [a,b,c:xs] = [(a,b,c)] ++ toTuple xs
//Start = toTuple [1,2,3,4,5,6,7,8,9]

tuple_sort :: [(Int,Int,Int)] -> [(Int,Int,Int)]
tuple_sort [] = []
tuple_sort list = toTuple (tlist list)

//Start = tuple_sort [(3,2,7),(1,6,8),(5,9,4)] //[(1,2,3),(4,5,6),(7,8,9)]
//Start = tuple_sort [(1,4,6),(9,2,3),(4,12,34)] // [(1,2,3),(4,4,6),(9,12,34)]
//Start = tuple_sort [(2,43,12),(45,6,3),(12,34,56),(3,2,1)] // [(1,2,2),(3,3,6),(12,12,34),(43,45,56)]
//Start = tuple_sort [] // []
//works for all
















