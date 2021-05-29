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

//another midterm 

/*
1. Given a list of lists of Int.
For each element greater than 5 of each sublist create a triple like (number, sum, product)
where
number -> the number itself
sum -> the sum of all the integers in the [1,number) interval
product -> product of all the integers in the [1,number)

Example:
[[1,2,4],[6,8,2,1]] -> [[],[(6,15,120),(8,28,5040)]]
because 1,2,4,2,1 are not greater than 5,
15 = 1+2+3+4+5, 120 = 1*2*3*4*5
28 = 1+2+3+4+5+6+7. 5040 = 1*2*3*4*5*6*7
*/

numaux :: [Int] -> [(Int,Int,Int)]
numaux [] = []
numaux [x:xs]
| x > 5 = [(x, sum [1..x-1], prod [1..x-1])] ++ numaux xs
= numaux xs
//Start = numaux [6,8,2,1]

numSumProd::[[Int]]->[[(Int,Int,Int)]]
numSumProd [] = []
numSumProd lsts = map numaux lsts

//Start = numSumProd [[1,2,4],[6,8,2,1]]//[[],[(6,21,720),(8,36,40320)]] should be [[],[(6,15,120),(8,28,5040)]]
//Start = numSumProd [[1..6],[1..5]]//[[(6,15,120)],[]]
//Start = numSumProd [[1,2,2],[]]//[[],[]]
//Start = numSumProd []//[]
/* done */

/*
2. Write a function that will do a circle rotation of the numbers in a list of tuples.
For example: rotate 1 [(1,2),(3,4),(5,6)] will give you [(2,3),(4,5),(6,1)]
*/

/*
rotateOnce :: [(Int,Int)] -> [(Int,Int)]
rotateOnce list = func list ++ [(snd (last list), fst (hd list))]
where
	func :: [(Int,Int)] -> [(Int,Int)]
	func [] = []
	func [x] = []
	func [x,y:xs] 
	= [ (snd x, fst y) : func [ y :xs] ]
*/  //this function rotates it once only

rotateOnce :: [(Int,Int)] -> [(Int,Int)]
rotateOnce list = func list ++ [(snd (last list), fst (hd list))]
where
	func :: [(Int,Int)] -> [(Int,Int)]
	func [] = []
	func [x] = []
	func [x,y:xs] 
	= [ (snd x, fst y) : func [ y :xs] ]
	
rotate :: Int [(Int,Int)] -> [(Int,Int)]
rotate n list = func 0 n list
where
	func x n list
	|x >= n = list
	= func (x+1) n (rotateOnce list)
Start = rotate 1 [(1,2),(3,4),(5,6)] //[(2,3),(4,5),(6,1)]
//Start = rotate 3 [(1,2),(3,4),(5,6)] //[(4,5),(6,1),(2,3)]
//Start = rotate 234 [(1,2),(3,4),(5,6)] //[(1,2),(3,4),(5,6)]
//Start = rotate 2378475 [(53,73),(35,71),(52,42),(56,78),(42,69),(457,1367),(32,283623),(-363,4643),(0,0),(35,-279427)] //[(4643,0),(0,35),(-279427,53),(73,35),(71,52),(42,56),(78,42),(69,457),(1367,32),(283623,-363)]


/*
3. Write a function which takes a list of lists
returns the product of all the elements of
the palindrom lists
example: [[1,2,1], [4,5], [1,2,3,2,1]] -> 24
because the lists ([1,2,1] and [1,2,3,2,1]) are
palindroms so we multiply all of their elements together
Note: If there are no palindroms, return 1
*/

isPali :: [a] -> Bool | == a
isPali [] = False
isPali lst = lst == reverse lst
//Start = isPali [1,2,4,2,1]

prodPali :: [[Int]] -> Int //| +a
prodPali [] = 1
prodPali [x:xs]
| isPali x = (prod x) * prodPali xs
= prodPali xs
//Start = prodPali [[1,2,1], [4,5], [1,2,3,2,1]] //24
//Start = prodPali [[1.0,11.0,2.0], [], [10.1,10.1]] // 102.01
//Start = prodPali [[1,3]] // 1
/* does not work for real */


/*
4. Consider following example:
"aaaabbbcaddd" -> [(4, 'a'), (3, 'b'), (1, 'c'), (1, 'a'), (3, 'd')]
i.e. consecutive duplicates are grouped together and their count is taken in a tuple
together with the character.
This method is called Run-length encoding.
Your task is to implement "encode" function which does this.
*/

//split :: [Char] -> [[Char]]
//split [] = []
//split [x:xs] = [x] ++ (split xs)
//Start = ['a','b','c']

// Start = encode "aaaabbbcaddd" // [(4, 'a'), (3, 'b'), (1, 'c'), (1, 'a'), (3, 'd')]
// Start = encode "" // []
// Start = encode "aa" // [(2, 'a')]
// Start = encode "abcde" // [(1, 'a'), (1, 'b'), (1, 'c'), (1, 'd'), (1, 'e')]


/*
7. Given a list of Integers. Write a function that creates a list of triples in the following way:
[1,2,3,4] -> [(1,2,3),(4,0,0)]
[1,2,3,4,5] -> [(1,2,3),(4,5,0)]
[1,2,3,4,5,6] -> [(1,2,3),(4,5,6)]
It basically groups 3 consecutive elements into a triple tuple.
*/

createTriples :: [Int]->[(Int,Int,Int)]
createTriples [] = []
createTriples [x] = [(x, 0, 0)]
createTriples [x,y] = [(x,y, 0)]
createTriples [x,y,z:xs] = [(x,y,z)] ++ createTriples xs 

//Start = createTriples [1..6]//[(1,2,3),(4,5,6)]
//Start = createTriples [1..5]//[(1,2,3),(4,5,0)]
//Start = createTriples [1..4]//[(1,2,3),(4,0,0)]
//Start = createTriples [2..15]//[(2,3,4),(5,6,7),(8,9,10),(11,12,13),(14,15,0)]
/* done */


/*
8. Write a function which takes a list of lists
of integers and returns the lists where
whether the difference between the maximum
and the average is bigger than or equal to a given
number
*/

diffAvgMax :: [[Int]] Int -> [[Int]]
diffAvgMax [] _ = []
diffAvgMax lsts n = [a\\ a<- lsts | ((maxList a) - (avg a)) >= n]

//Start = diffAvgMax [[1,2,4], [1,2]] 2 // [[1,2,4]]
//Start = diffAvgMax [[1]] 1 // []
//Start = diffAvgMax [[1..10], [20..30], [1..100]] 10 // [1..100]
/* done */

/*
10. Write evalIntExpr function which takes list of expressions
and returns list with their answers. Each expression is represented
with tuple - (Operator, Int, Int). Operator denotes type of expression
and 2 integers are arguments for it.
Operators:
* ADD a b - a+b : addition
* SUB a b - a-b : subtraction
* MULT a b - a*b : multiplication
* DIV a b - a/b : division
* REM a b - a%b : remainder
* CONC a b - ab : concatenation, example: CONC 120 71 -> 12071
*/
:: Operator = ADD | SUB | MULT | DIV | REM | CONC

instance == Operator 
where
	(==) ADD ADD = True
	(==) SUB SUB = True
	(==) MULT MULT = True
	(==) DIV DIV = True
	(==) REM REM = True
	(==) CONC CONC = True
	(==) _ _ = False
	
evalaux :: (Operator, Int, Int) -> Int
evalaux (o,x,y)
| o == ADD = x+y
| o == SUB = x-y
| o == MULT = x*y
| o == DIV = x/y
| o == REM = x rem y
| o == CONC = toInt((toString x) +++ (toString y))

evalIntExpr :: [(Operator,Int,Int)] -> [Int]
evalIntExpr [] = []
evalIntExpr [x:xs] = [evalaux x] ++ evalIntExpr xs
//Start = evalIntExpr [] // []
//Start = evalIntExpr [(ADD,12,17),(SUB,3,9),(MULT,2,3),(DIV,10,3),(REM,10,3),(CONC,120,71)] // [29,-6,6,3,1,12071]
//Start = evalIntExpr [(ADD,12,17),(CONC,0,5),(CONC,13,0),(MULT,1,1)] // [29,5,130,1]
/* done */


/*
5. Given the list of tuples. Each tuple has 3 element: L, R and Step.
For each tuple generate list of numbers from L to R increasing with Step (L,L+Step,L+2*Step...).
For example if L is 1, R is 10 and step is 4 list would be [1,5,9]. Your function should return
list of lists.
*/

expaux :: (Int,Int,Int) -> [Int]
expaux (l,r,s) = [a\\a <- [l..r] | a < r]
//Start = expaux (1,10,4)

//expandList :: [(Int,Int,Int)] -> [[Int]]
//expandList [] = []
//expandList [x:xs] = [a+ thd3 a\\l <- [l..] | l+s <= r] ++ expandList xs

//Start = expandList [(1,10,4), (3,5,4), (5,4,1), (1,10,3)] // [[1,5,9],[3],[],[1,4,7,10]]
// Start = expandList [] // []
// Start = expandList [(5,3,-1),(2,13,3),(1,8,1)] // [[5,4,3],[2,5,8,11],[1,2,3,4,5,6,7,8]]
// Start = expandList [(1,12,100), (2,5,10), (4,-1,-10)] // [[1],[2],[4]]


:: Month = January | February | March | April | May | June | July | August | September | October | November | December

montonum :: Month -> Int
montonum January = 1
montonum February = 2
montonum March = 3
montonum April = 4
montonum May = 5
montonum June = 6
montonum July = 7
montonum August = 8
montonum September = 9
montonum October = 10
montonum November = 11
montonum December = 12

numtomon :: Int -> Month
numtomon 1 = January
numtomon 2 = February
numtomon 3 = March
numtomon 4 = April
numtomon 5 = May
numtomon 6 = June
numtomon 7 = July
numtomon 8 = August
numtomon 9 = September
numtomon 10 = October
numtomon 11 = November
numtomon 12 = December

/*
6. Write a function that takes a list of months and sorts the list by order of the months.
Notably, January should be sorted before February, which should be sorted before March... and so on and so forth.
Duplicates can be kept
*/

monthSort :: [Month] -> [Month]
monthSort [] = []
monthSort lst = map numtomon (sort(map montonum lst))
//Start = monthSort [February, October, January, June, December, May, April, October] // [January,February,April,May,June,October,October,December]
//Start = monthSort [January, January, October, June, December, May, April, October] // [January,January,April,May,June,October,October,December]
//Start = monthSort [] //[]
/* done */






















