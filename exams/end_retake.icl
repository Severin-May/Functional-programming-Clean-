module end_retake
import StdEnv

// Please fill the data required below.
//<Name> Aichurok Kanatbekova
//<Neptun ID> RiIMR52
//Functional Programming & end-term-retake
//2021.January.11
//This solution was submitted and prepared by <Name, Neptun ID> for the end-term-retake assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither  made it public.
//Students’ regulation of Eötvös Loránd University (ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student’s work - 
//or at least the significant part of it - as his/her own performance, it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be dismissal of the student from the University.
/*
	1. Create a `toInt` instance for the Person record. An integer representation of a person
	is the sum of the length of its firstName, their age and height.
*/

::Person={firstName::String, age::Int, height::Int}
Rose::Person
Rose={firstName="Rose",age=23,height=172}
Jack::Person
Jack={firstName="Jack",age=25,height=193}
Emilia::Person
Emilia={firstName="Emilia",age=15,height=160}
Leo::Person
Leo={firstName="Leo",age=16,height=175}
Grace::Person
Grace={firstName="Grace",age=35,height=165}
Harry::Person
Harry={firstName="Harry",age=42,height=180}
Emilia2::Person
Emilia2={firstName="Emilia",age=15,height=180}


// TO DO instance
instance toInt Person
where
	toInt a = size(a.firstName) + a.age + a.height
//Start = toInt Rose // 199
//Start = toInt Leo // 194
//Start = toInt Grace // 205

/*
	2. Create an instance of `isEven` for the Person record. A person is even if the sum of their
	age and height is even.
*/
// TO DO instance
instance isEven Person
where
	isEven a
	| isEven (a.age + a.height) = True
	= False
//Start  = isEven Rose // False
//Start = isEven Harry // True

// 3.Given a list of continents, give back the names of the continents that have 
// at least one country whose capital has prime number of 'i' in it.

::Country={name::String,capital::String}
Macedonia::Country
Macedonia={name="Macedonia",capital="Skopje"}
Hungary::Country
Hungary={name="Hungary",capital="Budapest"}
Spain::Country
Spain={name="Spain",capital="Madrid"}
Brazil::Country
Brazil={name="Brazil",capital="Brasilia"}
Chile::Country
Chile={name="Chile",capital="Santiago"}
Argentina::Country
Argentina={name="Argentina",capital="Buenos Aires"}
China::Country
China={name="China",capital="Beijing"}
India::Country
India={name="India",capital="New Delhi"}
::Continent={contName::String,countries::{Country}}
Europe::Continent
Europe = {contName="Europe",countries={Macedonia,Hungary,Spain}}
Asia::Continent
Asia = {contName="Asia",countries={China,India}}
SouthAmerica::Continent
SouthAmerica ={contName="South America",countries={Argentina,Brazil,Chile}}

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n 
| isEmpty [x\\x<- [2..n-1] | n rem x == 0 && x <> n] = True
= False

/*
hasI :: Country -> Bool
hasI country 
|isPrime (length [x\\x <-: country.capital | x == 'i' || x == 'I']) = True
= False
*/

hasI :: String -> Bool
hasI capi
| isEven(sum( [1\\x <-: capi | x == 'i' || x == 'I'])) = True
= False
//Start = hasI "Skopje"

/*
hasI :: String -> [Char]
hasI capi = [x\\x <-: capi | x == 'i']
*/
//Start = hasI "Skopje"
ccc :: Continent -> [String]
ccc a = [x.capital\\ x <-: a.countries]

//Start = ccc Europe

lala :: Continent -> Bool
lala conti
|( isEmpty(filter hasI([b\\ b <- (ccc conti)]) ))= True
= False


//lala :: Continent -> [String]
//lala conti = ( (filter hasI ([b\\ b <- (ccc conti)]) ))

//Start = lala Asia
continentsPrimeI::[Continent]->[String]
continentsPrimeI conts = [x.contName\\x <- conts | (lala x)]
//Start=continentsPrimeI [Europe,Asia]//["Asia"]
//Start=continentsPrimeI [Europe]//[]
//Start=continentsPrimeI [Europe,SouthAmerica,Asia]//["South America","Asia"]



// 7. You are given array of integers.
// Your function should return true if each value appears at least twice in the array, and it should return false otherwise.

f1 :: Int [Int] -> Bool  
f1 x list = sum [1 \\ n <- list | n == x] >= 2
f2 :: [Int] -> [Int]
f2 list = removeDup[n \\ n <- list | f1 n list]
toList :: {Int} -> [Int]
toList arr = [x\\x<-: arr]
f7 :: {Int} -> Bool
f7 arr = f2 (toList arr) == removeDup (toList arr)
//Start = f7 {1,2,3,1,3,2,2,2} // True
//Start = f7 {1,2,3,4,3,2,1} // False
//Start = f7 {1,1,1,3,3,4,3,2,4,2} // True

/*
    10. 
    Implement the following methods of the Dictionary ADT.
	-keysNum
	-valueForKey
	-insert
	-remove
*/

::Dictionary a b:==[(a,b)]
dict::Dictionary String Int
dict=[("first",23),("second",234234),("third",21231)]
dict2::Dictionary String Int
dict2=[("a",1)]


/* a) keysNum - Calcualte the number of keys in the dictionary */

//keysNum::(Dictionary String Int)->Int


//Start=keysNum dict//3
//Start=keysNum dict2//1


/*
	b) valueForKey - Gives back the value associated with a given key.
	If the key is not in the dictionary return "The key is not in the dictionary"
*/

//valueForKey::(Dictionary String Int) String- > Int

//Start=valueForKey dict "first"//23
//Start=valueForKey dict "firstt"//The key is not in the dictionary


/*	c) insert-Inserts a new tuple if the key value is not in the dictionary already,
	or give back "The given key already exists" if the key is already in the dictionary
*/

//insert::(Dictionary String Int) (String,Int)->(Dictionary String Int)

//Start  = insert dict ("third",12312)//"The given key already exists"
//Start = insert dict ("fourth",1)//[("first",23),("second",234234),("third",21231),("fourth",1)]

/*	d) remove-remove the (key, value) pair for a given key.
	If the key is not in the dictionary return "The key is not in the dictionary"
*/

//remove::(Dictionary String Int) String->(Dictionary String Int)

//Start=remove dict "first"//[("second",234234),("third",21231)]
//Start=remove dict "someOtherKey"//The key is not in the dictionary
//-------------------------------------------------------------------------------


keysNum::(Dictionary String Int)->Int
keysNum [] = 0
keysNum [x:xs] = 1 + keysNum xs


valueForKey::(Dictionary String Int) String -> Int
valueForKey list k
|isEmpty [x \\ x<-list |fst x == k] = abort "The key is not in the dictionary"
= snd (hd [x \\ x<-list |fst x == k])


insert::(Dictionary String Int) (String,Int)->(Dictionary String Int)
insert list new
|isEmpty [x \\ x<-list |fst x == fst new] = list ++ [new]
= abort "The given key already exists"

remove::(Dictionary String Int) String->(Dictionary String Int)
remove list k
|isEmpty [x \\ x <- list | fst x == k] = abort "The key is not in the dictionary"
= [x \\ x <- list | fst x <> k]
//Start=remove dict "first"//[("second",234234),("third",21231)]
//Start=remove dict "someOtherKey"//The key is not in the dictionary

/* 4
Me and my friends went to play football in the streets, and the game ended as tie, so we were discussing 
if we should go for penalties or not. Help me to decide that.
You will get in a list each one of my team member skill Level and name, and you will get the name of 
the other team's goalkeeper and his/her level of skill.
If the skill of the player is greater or equal than the skill of the goalkeeper, then the penalty will count as scored.
The team would win this virtual game, if at least 3 or more penalties could be scored against the given goalkeeper.
*/

::APlayer = { name ::String, skillLevel :: Int}
shouldWePlay :: [APlayer] APlayer -> Bool
shouldWePlay list defender = help list defender >= 3
where
	help [ ] _ = 0
	help [x:xs] defender
	|x.skillLevel >= defender.skillLevel = 1 + help xs defender
	= help xs defender
	
//shouldWePlay :: [APlayer] APlayer -> Bool

// Start = shouldWePlay [{name = "kareem", skillLevel = 4},{name = "Tarek", skillLevel = 3},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} // False
// Start = shouldWePlay [{name = "kareem", skillLevel = 5},{name = "Tarek", skillLevel = 4},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} // True

//-------------------------------------------------------------------------------


/*
    6. Given a predefined MaybeInt type, define a new operator !+!
    for accessing the nth element in the list, you can test it with showFifthElement function.
*/

:: MaybeInt = Just Int | Nothing

// DEFINITION OF OPERATOR !+! ..... YOUR CODE COMES HERE....

//Just for testing purposed. DO NOT MODIFY
showFifthElement :: [Int] -> String
showFifthElement xs
  = case xs !+! 4 of
      Nothing -> "There is no fifth element in this list"
      Just n  -> "The fifth element of the list is: " +++ toString n

(!+!) infix 4 :: [Int] Int -> MaybeInt
(!+!) list n
|length list < 5 = Nothing
= Just (list !! n)

//Start = showFifthElement [1,2..10] // "The fifth element of the list is: 5"
// Start = showFifthElement [0,0] // "There is no fifth element in this list"
// Start = showFifthElement [33, 41, 56, 12, 96, 1] // "The fifth element of the list is: 96"

// 9.
// Write a filter function for colored rose tree.
// Colored rose Tree is a tree where each node has 
// some value, color and children nodes stored in list.
// Your filter function should take tree, color, a two 
// condition function and filtering type as an argument. Return a list of
// values stored in nodes which have given color and
// satisfy both of the given conditions if filter type is 'AND'
// or satisfy at least one of the given functions if filter type
// is "OR" (Condition function returns
// true for node's value).

:: NodeColor = Red | Green | Blue

:: FilterType = AND | OR

:: ColoredRoseTree a = Node a NodeColor [ColoredRoseTree a] | Leaf


// TODO
//filterColoredTree :: (ColoredRoseTree a) NodeColor FilterType (a -> Bool) (a -> Bool) -> [a]


tree1 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Leaf])]
tree2 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Node 7 Red [Node 9 Red [], Node 10 Red []]])]


instance == NodeColor
where
    (==) Red Red = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) _ _ = False
    
instance == FilterType
where
	(==) AND AND = True
	(==) OR OR = True
	(==) _ _ = False

filterColoredTree :: (ColoredRoseTree a) NodeColor FilterType (a -> Bool)(a -> Bool) -> [a]
filterColoredTree Leaf  _ _ _ _ = []
filterColoredTree (Node x c children) col fT pred pred2 = curr ++ rans
where
    rans = flatten [filterColoredTree child col fT pred pred2 \\ child <- children]
    curr
    |fT == AND && pred x && pred2 x && col == c = [x] 
    |(pred x || pred2 x) && col == c = [x]
    = []

// Start = filterColoredTree tree1 Blue OR isEven isOdd // [2,4,3]
// Start = filterColoredTree tree1 Blue AND isEven isOdd // []
// Start = filterColoredTree tree1 Blue AND isOdd isOdd // [3]
// Start = filterColoredTree tree2 Red OR (\x = True) isEven // [1,7,9,10]
// Start::[Int] // Uncomment this line too, to run next test
// Start = filterColoredTree Leaf Green OR isOdd isEven // []
//-------------------------------------------------------------------------------


//another endterm


module kairat
import StdEnv


/* 3

 Create an instances +, -, <, == for RGBColor

 + should add respective parameters

 - should subtract respective parameters

 == is true if all thre parameters are equal

 < Compare them lexicographically (if reds are equal compare greens and so on)

*/

:: RGBColor = { r :: Int, g :: Int, b :: Int}

instance + RGBColor
where
	(+) a b = {r = a.r+b.r, g = a.g+b.g, b = a.b+b.b}

instance - RGBColor
where
	(-) a b = {r = a.r-b.r, g = a.g-b.g, b = a.b-b.b}

instance == RGBColor
where
	(==) a b
	| a.r == b.r && a.g == b.g && a.b == b.b = True
	= False
	
instance < RGBColor
where
	(<) a b
	| a.r == b.r = a.g <> b.g 
	| a.r == b.r && a.g == b.g = a.b <> b.b 
	= False

//Start = {r = 0, g = 0, b = 0} == {r = 0, g = 0, b = 0} // True
//Start = {r = 0, g = 0, b = 0} <> {r = 0, g = 0, b = 0} // False
// Start = {r = 30, g = 150, b = 231} == {r = 10, g = 30, b = 231} // False
//Start = {r = 30, g = 150, b = 231} - {r = 1, g = 1, b = 1} // {r = 29, g = 149, b = 230}
//Start = {r = 30, g = 150, b = 231} + {r = 1, g = 1, b = 1} // {r = 31, g = 152, b = 232}
//Start = {r = 30, g = 150, b = 231} < {r = 10, g = 30, b = 231} // False
//Start = {r = 30, g = 150, b = 231} < {r = 30, g = 150, b = 231} // False
//Start = {r = 30, g = 150, b = 231} < {r = 30, g = 151, b = 231} // True



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



/* 5

 Create an instance `-` for the record Course, 

 such that two records A and B when subtracted (A-B) will give

 a record Course whose name is the first letter of A's name, 

 major is B's major and credits is A's credits

 minus B's credits

*/

//:: Course = {name::String, major::Major, credits::Int}

instance - Course
where
	(-) a b = {name = toString(a.name.[0]), major = b.major, credits = a.credits - b.credits}


//Start = Morphology - Quantum_mechanics // (Course "M" Physics -1)

//Start = Corporate_finance - OOP // (Course "C" CS 1)

//Start = Quantum_mechanics - Relativity // (Course "Q" Physics -2)


/* 9

 Having a DAY algebraic data type, write a function

 to output the day after n days of the day.

 for example: MONDAY 2 -> WEDNESDAY

*/

daytonum :: DAY -> Int
daytonum MONDAY = 0
daytonum TUESDAY = 1
daytonum WEDNESDAY = 2
daytonum THURSDAY = 3
daytonum FRIDAY = 4
daytonum SATURDAY = 5
daytonum SUNDAY = 6

numtoday :: Int -> DAY
numtoday 0 = MONDAY 
numtoday 1 = TUESDAY 
numtoday 2 = WEDNESDAY
numtoday 3 = THURSDAY
numtoday 4 = FRIDAY 
numtoday 5 = SATURDAY
numtoday 6 = SUNDAY


:: DAY = MONDAY | TUESDAY | WEDNESDAY | THURSDAY | FRIDAY | SATURDAY | SUNDAY

getDay :: DAY Int -> DAY
getDay day n = numtoday (((daytonum day) + n) rem 7 )

//Start = getDay MONDAY 0 // MONDAY //0+0 = 0 rem 7= 0 
//Start = getDay MONDAY 2 // WEDNESDAY //0+2 = 2 rem 7 = 2
//Start = getDay FRIDAY 8 // SATURDAY //4+8= 12 rem 7 = 5
//Start = getDay TUESDAY 300 // MONDAY //1+300 rem 7 = 0


/* 7

 A coach is trying to choose the player of the year, in his team

 so let's help him/her doing that, the coach wants the player to have the

 highest overall.

 The coach is adding 0.1 to the overall if the player was good.

 NOTE: If there are two players with the highest overall, choose any.

*/


::Status = Good | Bad

::Player = {player_name :: String, overall :: Real, status :: Status}

instance == Status
where
	(==) Good Good = True
	(==) Bad Bad = True
	(==) _ _ = False


changeOver :: Player -> Player
changeOver p
| p.status == Good = {p & overall = p.overall  + 0.1}
= p
//Start = changeOver {player_name = "Khalid", overall = 4.0, status = Good}

instance < Player
where
	(<) a b = a.overall < b.overall

bestPlayer :: [Player] -> Player
bestPlayer [x:xs] = maxList [changeOver p\\p <- [x:xs] | p.status == Good]


//Start = bestPlayer [{player_name = "Khalid", overall = 4.0, status = Good},{player_name = "Peter", overall = 3.7, status = Good},{player_name = "Yoko", overall = 2.9, status = Bad}] // (Player "Khalid" 4.1 Good)
//Start = bestPlayer [{player_name = "Khalid", overall = 3.7, status = Bad},{player_name = "Peter", overall = 3.7, status = Good},{player_name = "Yoko", overall = 2.9, status = Bad}] // (Player "Peter" 3.8 Good)



/* 1

 Given an array of tuples of empty or single element lists.

 Give back a list of arrays in the following form

 {([2],[1]),([3],[4]),([5],[])} -> [{2,1},{3,4},{5,0}]

*/

conv :: ([Int],[Int]) -> {Int}
conv ([],[]) = {0,0}
conv (a, []) = {hd a, 0}
conv ([],b) = {0, hd b}
conv (a,b) = {hd a,hd b}
//Start = conv ([2],[])

toList :: {a} -> [a]
toList array = [a\\a<-: array]

conversion::{([Int],[Int])} -> [{Int}]
conversion array = map conv (toList array)

//Start = conversion {([2],[1]),([3],[4]),([5],[])}//[{2,1},{3,4},{5,0}]
//Start = conversion {([],[]),([],[4]),([5],[])}//[{0,0},{0,4},{5,0}]
//Start = conversion {([4],[])}//[{4,0}]


/* 2

 Given an array of arrays of triples of Ints and a condition over triples of Ints.

 Return the index of the subarray having the highest number of triples satisfying the condition.

 In case there are more subarrays with the same number return the first one.

 indexing starts from 1 (i+1).



 Example:

 {{(1,2,3)},{(2,2,2),(3,3,3),(3,3,3)},{(1,1,1)}} condition: all 3 numbers are the same ->

 first subarray - 0

 second subarray - 3

 third subarray -1 

 Solution: 2 (because the second array has the highest count)

*/

condit :: {(Int,Int,Int)} ((Int,Int,Int)->Bool) -> Int
condit arr condi = sum[1\\x <-: arr | condi x ]
//Start = condit {(2,2,2),(3,3,3)} cond1 

mostTriplesCond::{{(Int,Int,Int)}} ((Int,Int,Int)->Bool) -> Int
mostTriplesCond arrays condi = (maxList[condit x condi \\x<-: arrays]) //hd[1\\y <-[condit x condi \\x<-: arrays]| y==(maxList[condit x condi \\x<-: arrays])] 
//The cond1 and cond2 functions are used in the start cases, please don't delete them


cond1::(Int,Int,Int) ->Bool
cond1 (a,b,c)= a==b && b==c
//Start = mostTriplesCond {{(1,2,3)},{(2,2,2),(3,3,3)},{(1,1,1)}} cond1//2
//Start = mostTriplesCond {{(1,2,3)},{(2,2,2),(3,3,3)},{(2,2,2),(3,3,3)},{(1,1,1)}} cond1 //2
cond2::(Int,Int,Int) ->Bool
cond2 (a,b,c)= a<b && b==c
//Start = mostTriplesCond {{(1,2,3)},{(2,2,2),(3,3,3)},{(2,2,2),(3,3,3)},{(0,1,1)}} cond2 //4


// 6 :: Major = Finance | CS | Math | Physics | Economy | Linguistics

instance == Major
where
	(==) Finance Finance = True
	(==) CS CS = True
	(==) Math Math = True
	(==) Physics Physics = True
	(==) Economy Economy = True
	(==) Linguistics Linguistics = True
	(==) _ _ = False
	
majtonum :: Major -> Int
majtonum Finance = 323
majtonum CS = 375
majtonum Math = 350
majtonum Physics = 369
majtonum Economy = 358
majtonum Linguistics = 320



value :: [Course] -> [(String, Real)]
value [] = abort "None"
value [x:xs] = [(x.name, toReal((majtonum x.major)/15/5)/toReal(x.credits))\\x <- [x:xs]]

//Start = value [Corporate_finance, OOP, Microeconomics]

instance < (String, Real)
where
	(<) x y = (snd x) < (snd y)

valuable :: [Course] -> (String, Real)
valuable [] = abort "None"
valuable list = maxList (value list)

//Start = valuable [Corporate_finance, OOP, Microeconomics] // ("OOP",1)

//Start = valuable [Morphology, Macroeconomics, Quantum_mechanics] // ("Morphology",1.42222222222222)

//Start = valuable [Venture_captical, Relativity, Cryptography] // ("Cryptography",2.5)

//Start = valuable [Discrete_math] // ("Discrete_math",1.16666666666667)

//Start = valuable [] // "None"













