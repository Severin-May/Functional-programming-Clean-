module mid_retake
import StdEnv

// Please fill the data required below.
//< Aichurok Kanatbekova
//<RIMR52
//Functional Programming & mid-term-retake
//2021.January.11
//This solution was submitted and prepared by <Name, Neptun ID> for the mid-term-retake assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither  made it public.
//Students’ regulation of Eotvos Lorand University (ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student’s work - 
//or at least the significant part of it - as his/her own performance, it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be dismissal of the student from the University.

/* 1.
    Given two sorted lists, return lists which contains their intersection.
*/

intersection :: [Int] [Int]-> [Int]
//intersection [] [] = []
intersection l1 l2 = [x\\x<- l1 |(isMember x l2) ]

//Start = intersection [1,3,4,7,8,12,13] [1,4,6,10,11,12,15] // [1,4,12]
//Start = intersection [1..10] [1..1000] // [1,2,3,4,5,6,7,8,9,10]
//Start = intersection [1,2,3,7,9,12,15,24] [5..30]  // [7,9,12,15,24]
//Start = intersection [] [1..100] // []
//Start = intersection [] []

/* 3.
    Write a function which takes a list of Int and returns a list where each number of the list 
    is substituted with the number of its divisors.
*/

div :: Int -> Int
div x = length [n \\ n <- [1..x] | x rem n == 0]

substWithNumOfDiv::[Int]->[Int]
substWithNumOfDiv list = [div n \\ n <- list]
//Start=substWithNumOfDiv [3,5,2,34,54,23,14]//[2,2,2,4,8,2,4]
//Start=substWithNumOfDiv [8,8,8,8]//[4,4,4,4]
//Start=substWithNumOfDiv [54,23,1,0]//[8,2,1,0]
//Start=substWithNumOfDiv []//[]

/* 2.
    Given an integer, write a function which reverse it's digit, assuming that it does not end with zero.
    1234 -> 4321
*/

reverseInt :: Int -> Int
reverseInt x = (foldr concat 0 (intToList x)) / 10
concat :: Int Int -> Int
concat x y = toInt(toString x +++ toString y)
intToList :: Int -> [Int] //reverses as well
intToList x
|not(x == 0) && x / 10 == 0 = [x]
= [(x rem 10) : (intToList (x/10))]

//Start = reverseInt 123 // 321
//Start = reverseInt 10152 // 25101
//Start = reverseInt 5 // 5


/* 4.
	Write a function which generates a list of the first n leap years starting from
	a year x. If any of the parameters are negative output empty list.
	A leap year is divisible by 4 but is NOT divisible by 100 UNLESS it is divisible by 400.
*/

leapYear :: Int -> Int
leapYear x 
|(x rem 4 == 0) && (x rem 100 <> 0) || (x rem 400 == 0) = x
= leapYear (x + 1)

LeapYears :: Int Int -> [Int]
LeapYears n 0 = []
LeapYears n k 
|k < 0 || n < 0 = []
= [leapYear (n + 1): LeapYears(leapYear (n+1)) (k-1)]

//Start = LeapYears 1999 4 // [2000,2004,2008,2012]
//Start = LeapYears 1804 7 //[1808,1812,1816,1820,1824,1828,1832]
//Start = LeapYears -2000 4 //[]
//Start = LeapYears 2000 -9//[]

/*  5.
    Find sum square difference of a given integer
    The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + ... + 10^2 = 385
    The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^2 = 55^2 = 3025
    Hence the difference between the sum of the squares of the first ten natural numbers 
    and the square of the sum is: 3025 - 385 = 2640.
*/
sqSum :: Int -> Int
sqSum n = sum [x^2\\x <- [1..n]]
//Start = sqSum 100
sumOfSquare :: Int -> Int
sumOfSquare n = (((sum ([1..n]))^2) - (sqSum n))
//Start = sumOfSquare 100 // 25164150
//Start = sumOfSquare 50 // 1582700


/* 7. 
    Write a function to decide if all the numbers in a list are perfect square.
    INFO : Perfect number is an integer equals to a square of another integer.
    i.e : 9 is a perfect square since 3^2 = 9.
*/

perfect :: Int -> Bool
perfect x = (sqrt (toReal x) * (sqrt (toReal x))) == toReal x

areAllPerfectSquares :: [Int] -> Bool
areAllPerfectSquares [] = True
areAllPerfectSquares [x:xs]
|perfect x = True && areAllPerfectSquares xs
= False
// Start = areAllPerfectSquares [1, 9, 36, 16] // True
// Start = areAllPerfectSquares [2, 4, 34] // False

/* 8.
    Given two numbers, find their common prime divisors.
*/

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = isP 2 n

isP :: Int Int -> Bool
isP i n
| n == i = True
= (n rem i) <> 0 && isP (i+1) n

getCommonPrimeDivs:: Int Int -> [Int]
getCommonPrimeDivs x y = [ common \\ common <- [1..x] | x rem common == 0 && y rem common == 0 && isPrime common ]

//Start = getCommonPrimeDivs (7*9*13*17*2) (14*7*3*17*19) // [2,3,7,17]
//Start = getCommonPrimeDivs 1234 7790 // [2]
//Start = getCommonPrimeDivs 124 8139 // []


/* 6.
    Given a list of Integers. Write a function which will remove the numbers which do not belong to the first 15 Fibonacci numbers.
*/

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

f2 :: Int -> [Int]
f2 n = [fib x \\ x <- [1..15]]
//Start = f2 15
removeNonFib :: [Int] -> [Int]
removeNonFib [] = []
removeNonFib list = [x \\ x <- list |(isMember x (f2 15))]
//Start = removeNonFib [1,3..20]//[1,3,5,13]
//Start = removeNonFib [2,4,6,8,10]//[2,8]
//Start = removeNonFib [4,6,7,9,10]//[]
//Start = removeNonFib [] //[]

/* 10.
    Write a function that takes a list of rectangles represented
    as tuples of their side lengths, and then return the largest
    rectangle (tuple) by area. For multiple rectangles (tuples)
    with equal area, return any of them.
    For example:
    largestRect [(1,1),(2,3),(4,6)] should return (4,6) because
    that will give the largest rectangle by area.
    Math hint:
    Given two side lengths, you get the area of a rectangle by
    multiplying the two lengths.
*/

largestRect :: [(Int,Int)] -> (Int,Int)
largestRect [] = (0,0)
largestRect list = hd [x \\ x <- list | getMax list == (fst x * snd x)]
getMax :: [(Int,Int)] -> Int
getMax list = maxList [ fst x * snd x \\ x <- list]

//Start = largestRect [] //(0,0)
//Start = largestRect [(420,69)] //(420,69)
//Start = largestRect ([(1,1),(2,3),(4,6)])//(4,6)
//Start = largestRect [(1,2),(2,3),(3,4),(2,6),(2,1)] //(2,6) //Solution gives (3,4) because area is same with max!






