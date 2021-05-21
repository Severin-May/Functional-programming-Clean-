module hw01
import StdEnv

// task 1
// Write a function that will take a digit (Int)
// and return the respective word for it (String).
// For example input of 1 should output One; input of 0 should output Zero; input of 5 should output Five.
// Anything that is not the digit (0-9) should output "Not a digit"

digit_to_string :: Int -> String
digit_to_string n
| n < 0 || n > 9 = abort "Not a digit"
| n == 0 = "Zero"
| n == 1 = "One"
| n == 2 = "Two"
| n == 3 = "Three"
| n == 4 = "Four"
| n == 5 = "Five"
| n == 6 = "Six"
| n == 7 = "Seven"
| n == 8 = "Eight"
| n == 9 = "Nine"
//Start = digit_to_string 4 //"Four"
//Start = digit_to_string 0 //"Zero"
//Start = digit_to_string 5 //"Five"
//Start = digit_to_string 10 //"Not a digit"
//Start = digit_to_string -1 //"Not a digit"
//Start = digit_to_string 42 //"Not a digit"

// task 2
// Write a function that takes Int and checks if this number is prime or not.
// handle the case of negative numbers (negative numbers are not primes).
// 0 and 1 are not prime numbers.

is_prime :: Int -> Bool
is_prime 0 = False
is_prime 1 = False
is_prime n
| isEmpty [x\\x <- [2..n]| n rem x == 0 && n <> x] = True
// n = 13     2 <- [2..13] | 13 rem 13 == 0 && 13 <> 13
// []
// n = 10     3 <- [2..10] | 10 rem 10 == 0 && 10 <> 10
// [2, 5] = False
= False
//Start = is_prime 5 // True
//Start = is_prime 0 // False
//Start = is_prime 1 // False
//Start = is_prime 2 // True
//Start = is_prime 2017 // True

//Start = [x\\x <- [1..1000]| is_prime x] 

/*
prime :: Int Int -> Bool
prime i k
| k == i = True // 23 == 23 = True stop condition
= (k rem i) <> 0 && prime (i+1) k 
//Start = isP 2 4              

is_prime :: Int -> Bool
is_prime 1 = False
is_prime n = prime 2 n
//Start = is_prime 2
*/

// task 3
// Write a function that takes Int argument and checks if this number is a palindrome.
// Palindrome is a number that is the same when we read from left to right or from right to left.

split :: Int -> [Int]
split 0 = []
split n = split (n/10) ++ [n rem 10]
//Start = reverse (split -1984)
 
is_palindrome :: Int -> Bool
is_palindrome n
| (split n) == (reverse (split n)) = True
= False

//Start = is_palindrome 0 // True
//Start = is_palindrome 55 // True
//Start = is_palindrome 49594 // True
//Start = is_palindrome 1337 // False
//Start = is_palindrome 12345678987654320 // False

// list comprehension
//Start = [x-1     \\ x <- [1..20] | isEven x]
//       output   listtin elementi  condition
// if(x % 5 == 0)
// printf(x)

// task 3
// Write a function that takes Int argument and checks if this number is a palindrome.
// Palindrome is a number that is the same when we read from left to right or from right to left.

// palindrome 0-9  11 12 13 22 33 
// is_palindrome :: Int -> Bool
// 1  = True
// 131
// 1005001 -> [1,0,0,5,0,0,1] 
// Start = reverse [1,0,0]
// 131 rem 10 = 1 
// 131/10 = 13
// 13 rem 10 = 3
// 13/10 = 1
// 1 rem 10 = 1

intToList :: Int -> [Int]
intToList 0 = []
intToList n = [n rem 10: intToList (n/10)]
//Start = intToList 1

is_palindrome1 :: Int -> Bool
is_palindrome1 n = (intToList n) == reverse (intToList n)
//Start = is_palindrome1 123321 
// [1,2,3] == [3,2,1]


// this function checks if all the elements of list are even
// [1,2,3,4] = False
// [2,4,6] = True
// [] = True
// [2..10] = False
// && recursion isEven 
//checkEven :: [Int] -> Bool

checkEven :: [Int] -> Bool
checkEven [] = True
checkEven list
|and[(isEven x)\\x <-list ] = True
= False
//Start = checkEven [1..21]





























