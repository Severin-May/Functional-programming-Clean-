module end
import StdEnv

/*
1.Given two integer numbers k and n.
Generate a String by the following pattern:
In case k=2 and n=3
"[[**[[**[[**"
So n times, k opening brackets followed by k stars.
*/


bracketsStars :: Int Int -> String
bracketsStars k n = toStr [""+++ x\\x<-(repeatn n (toString (take k ['[','['..]) +++ toString (take k ['*','*'..])))]

toStr :: [String] -> String
toStr [] = " " 
toStr[x:xs] = x +++ toStr xs
//Start = toStr ["&","&","&","&"]/

//bracketsStars :: Int Int -> String
//bracketsStars k n = toStr(repeatn n (toStr(repeatn k("[") ++ repeatn k("*"))))

//Start = bracketsStars 2 3 // "[[**[[**[[**"
//Start = bracketsStars 0 3 // ""
//Start = bracketsStars 5 2 // "[[[[[*****[[[[[*****"




/*
2.Write a function which takes a String and modifies it such that
only the first character is uppercase and every other one is lowercase.
The given string contains only letters!

Hints:
toInt('a') - toInt('A') == 32
Uppercase letters are in the range [65,90] (ASCII)
Lowercase letters are in the range [97,122] (ASCII)

Example:
"abcdFeH" = "Abcdfeh"
*/

arrtoList s = [x \\ x <-: s]
makeUpper :: Char -> Char
makeUpper x
|97 <= toInt(x) && toInt(x) <= 122 = toChar(toInt x - 32)
= x
makeLower :: Char -> Char
makeLower x
|65 <= toInt(x) && toInt(x) <= 90 = toChar(toInt x + 32)
= x
capitalize::String ->String
capitalize "" = ""
capitalize s = { x\\x<-[makeUpper (s.[0])]} +++ {makeLower x \\ x <- tl (arrtoList s) }

//Start = capitalize "aABCDEFG" // "Aabcdefg"
//Start = capitalize "abABab" // "Ababab"
//Start = capitalize "" // ""


:: Major = Finance | CS | Math | Physics | Economy | Linguistics
:: Course = {name::String, major::Major, credits::Int}

OOP::Course
OOP = {name="OOP",major=CS, credits=5}
Discrete_math::Course
Discrete_math = {name="Discrete_math",major=Math, credits=4}
Relativity::Course
Relativity = {name="Relativity", major=Physics, credits=6}
Functional::Course
Functional = {name="Functional", major=CS, credits=5}
Quantum_mechanics::Course
Quantum_mechanics = {name="Quantum_mechanics", major=Physics, credits=4}
Corporate_finance::Course
Corporate_finance = {name="Corporate_finance", major=Finance, credits=6}
Venture_captical::Course
Venture_captical = {name="Venture_captical", major=Finance, credits=6}
Macroeconomics::Course
Macroeconomics = {name="Macroeconomics", major=Economy, credits=6}
Microeconomics::Course
Microeconomics = {name="Microeconomics", major=Economy, credits=6}
Numerical_Methods::Course
Numerical_Methods = {name="Numerical_Methods", major=Math, credits=4}
Cryptography::Course
Cryptography = {name="Cryptography", major=CS, credits=2}
Phonology::Course
Phonology = {name="Phonology", major=Linguistics, credits=3}
Morphology::Course
Morphology = {name="Morphology", major=Linguistics, credits=3}

/*
3.Given a list of Courses and a major, check if any of the courses in the list
has the same major with the given one, return True if there is at least one.
*/

instance == Major
where
	(==) CS CS = True
	(==) Math Math = True
	(==) Physics Physics = True
	(==) Economy Economy = True
	(==) Linguistics Linguistics = True
	(==) Finance Finance = True
	(==) _ _ = False
	 
same_major :: [Course] Major -> Bool
same_major [] _ = False
same_major courses major
| (length ([c\\c <- courses | c.major == major]) ) > 0 = True
= False

//Start = same_major [Corporate_finance, OOP, Microeconomics] Finance // True
//Start = same_major [Morphology, Macroeconomics, Quantum_mechanics] CS // False
//Start = same_major [Venture_captical, Relativity, Cryptography] Physics // True
//Start = same_major [Discrete_math] Economy // False
//Start = same_major [] Physics // False
/* works */



/*
4.Given a list of Courses that a student has taken, write a function that
returns a list of records (you need to define by yourself, called Earned_credits),
each records has the name of the major (field) and
the credits the student earned for each mojor (earned).
*/

:: Earned_credits = {m_name:: Major, earned :: Int}

/*
group :: Earned_credits [Earned_credits] -> Earned_credits
group earn [] = earn
group earn creds = hd [{m_name = earn.m_name, earned = earn.earned + c.earned} \\ c <- creds | c.major == earn.major]
Start = group {m_name= CS, earned = 5} [{m_name= CS, earned= 5},{m_name= Physics, earned= 6}]
*/

calc :: [Course] -> [Earned_credits]
calc [] = []
calc courses = [{m_name = c.major, earned =  c.credits}\\ c <- courses]
//Start = calc [Functional, OOP, Relativity] // [(Earned_credits CS 10),(Earned_credits Physics 6)]
//Start = calc [Morphology, Macroeconomics, Numerical_Methods] // [(Earned_credits Math 4),(Earned_credits Economy 6),(Earned_credits Linguistics 3)]
//Start = calc [Corporate_finance, Numerical_Methods, Cryptography] // [(Earned_credits Finance 6),(Earned_credits Math 4),(Earned_credits CS 2)]
//Start = calc [] // []
/* needs to recheked*/



/*
5.Write '+' operator for lists.
If both lists are sorted in increasing order you should merge them
in a way that resulting list is sorted too.
Ex.: [1,3,6] + [2,4,5,7] -> [1,2,3,4,5,6,7]
If list is not sorted than it is considered empty.
Ex.: [1,3,6] + [2,3,1] -> [1,3,6] + [] -> [1,3,6]
Ex.: [2,9,7] + [5,4,3] -> [] + [] -> []
*/
isSorted :: [a] -> Bool | Ord a
isSorted [] = False
isSorted lst 
| lst == (sort lst) = True
= False
Start = isSorted [1,2,3]


instance + [a] | Ord a 
where
	(+) [] [] = []
	(+) [] [b:bs] = [b:bs]
	(+) [a:as] [] = [a:as]
	(+) [a:as][b:bs]
	| isSorted [a:as] && not (isSorted [b:bs]) = [a:as]
	| isSorted [b:bs] && not (isSorted [a:as]) = bs
	| isSorted [a:as] && isSorted [b:bs] = sort ([a:as] ++ [b:bs])
	
//Start = [1,2,3] + [1,3,6] // [1,1,2,3,3,6]
//Start = [1,3,6] + [2,4,5,7] // [1,2,3,4,5,6,7]
//Start = [1,3,6] + [2,3,1] // [1,3,6]
//Start = [5,1] + [1,3,6] // [1,3,6]
//Start = [] + [1] // [1]
//Start = [2,3,1] + [12,3,1] // []
//Start = [] + [] // []
/* works but needs to recheked */


:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))
tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))
tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))
tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))
tree5 :: Tree Int
tree5 = Node 1 tree3 tree4

/*
6.Define == for Tree. Two Trees are equal if each node value
from 1st tree is equal to 2nd tree.
*/

instance == (Tree a) | == a
where	
	(==) x y =  length(toList x) == length (toList y) && and[xx == yy \\ xx <- (toList x) & yy <- (toList y)]
	
toList :: (Tree a) -> [a]
toList Leaf = []
toList (Node x l r) = [x] ++ (toList l) ++ (toList r) 

//Start = tree1 == tree1 // True
//Start = tree2 == tree3 // False
//Start = tree4 == tree4 // True
//Start = tree1 == tree5 // False
/* works */

/*
9.Complementary colors are pairs of colors which, when combined or mixed,
cancel each other out (lose hue) by producing a grayscale color like white or black.
Such pairs are:
Red - Green
Orange - Blue
Yellow - Purple
Violet - Amber
Teal - Vermilion
Magenta - Chartreuse
Create an instance == for the Color and
write a function that finds complement of a given color.
*/

:: Color = Red | Yellow | Green | Blue | Purple | Orange | Violet | Amber | Teal | Vermilion | Magenta | Chartreuse

instance == Color
where
	(==) Red Red = True
	(==) Orange Orange = True
	(==) Yellow Yellow = True
	(==) Violet Violet = True
	(==) Teal Teal = True
	(==) Magenta Magenta = True
	(==) Green Green = True
	(==) Blue Blue = True
	(==) Purple Purple = True
	(==) Amber Amber = True
	(==) Vermilion Vermilion  = True
	(==) Chartreuse Chartreuse = True
	(==) _ _ = False

find_complement :: Color -> Color
find_complement col
| col == Red = Green
| col == Green = Red
| col == Orange = Blue
| col == Blue = Orange
| col == Yellow = Purple
| col == Purple = Yellow
| col == Violet = Amber
| col == Amber = Violet
| col == Teal = Vermilion
| col == Vermilion = Teal
| col == Magenta = Chartreuse 
| col == Chartreuse = Magenta
//Start = find_complement Red // Green
//Start = find_complement Green // Red
//Start = find_complement Teal // Vermilion
//Start = find_complement Chartreuse // Magenta
//Start = find_complement Violet // Amber
/* works */










