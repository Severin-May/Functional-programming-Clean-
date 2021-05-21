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

