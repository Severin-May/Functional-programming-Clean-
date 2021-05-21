module end_prep
import StdEnv

:: Human = {firstName::String, age::Int, height::Int}
Rose :: Human
Rose = {firstName="Rose", age=23, height=172}
Jack :: Human
Jack = {firstName="Jack", age=25, height=193}
Emilia :: Human
Emilia = {firstName="Emilia", age=15, height=160}
Leo :: Human
Leo = {firstName="Leo", age=16, height=175}
Grace::Human
Grace = {firstName="Grace", age=35, height=165}
Harry :: Human
Harry = {firstName="Harry", age=42, height=180}
Harry2 :: Human
Harry2 = {firstName="Harry", age=53, height=177}
Emilia2 :: Human
Emilia2 = {firstName="Emilia", age=25, height=180}
Emilia3 :: Human
Emilia3 = {firstName="Emilia", age=17, height=186}

/*
	1. Create an instance of `==` for the Human record. Two people are equal if
    name is same and the age difference is not more than 10 years.
*/
instance == Human
where
	(==) a b = a.firstName == b.firstName && abs(a.age - b.age) < 10
	
//Start = Leo == Rose // False
//Start = Harry == Harry // True
//Start = Emilia == Emilia2 // False
//Start = Emilia2 == Emilia3 // True
//Start = Harry == Harry2 // False
/* works for all */

/*
	2. Given a list of tuples [(Int,Int)]. Overload the * operator on [(Int,Int)] such that:
    1) we multiply all the tuples in both lists (get one tuple for each list).
    2) we add resulted tuples (we will have one tuple in each list after applying the previous) 
	and store resulted tuple in a list.
    Note 1: (a,b) * (c,d) = (a*c, b*d)
    Note 2: (a,b) + (c,d) = (a+c, b+d)
	Note: if one of arguments is [] then return []
*/
a :: [(Int,Int)] -> Int
a [] = 0
a [x:xs] = prod [fst x\\ x <- [x:xs]]
b :: [(Int,Int)] -> Int
b [] = 0
b [x:xs] = prod [snd x\\ x <- [x:xs]]

instance * [(Int,Int)]
where
    (*) [] _ = []
    (*) _ [] = []
	(*) x y = [((a x)+(a y),(b x)+(b y))]
//Start = [(1,2)]*[] // []
//Start = [(1,2), (3,4), (5,6)] * [(3,5)] // [(18,53)]
//Start = [(1,2), (3,4), (5,6)] * [(3,5),(6,2),(4,5),(9,7)] // [(663,398)]
//Start = [(1,2), (2,1), (3,2)] * [(2,2),(0,0)] // [(6,4)]
/*works for all */

/*
	3. Given an array of lists of Ints and an Int, keep the lists whose difference between 
	max and mean (average) is less than the given number. There are no [] in the array
*/

mean_max_diff :: {[Int]} Int -> {[Int]}
mean_max_diff arr n = {x\\x<-: arr | abs((maxList x)-(avg x)) <= n}
//Start = mean_max_diff {[1,21,2],[1,1,1,1,1],[1]} 5//{[1,1,1,1,1],[1]}
//Start = mean_max_diff {[1,21],[1..10],[4,3]} 5//{[1,2,3,4,5,6,7,8,9,10],[4,3]}
//Start = mean_max_diff {[1..10],[5..345]} -3//{}
/*works for all */

/*
	4. Given array find minimum of it and return new array which has just all occurrences of maximum (keep maximum only).
	For example, if given array is {1,4,5,3,3,2,4,5,1,3,4}, maximum is 5, so answer should be {5,5}.
*/
toList :: {Int} -> [Int]
toList arr = [x\\x<-: arr]
//Start = toList {1,4,5,3,3,2,4,5,1,3,4}
keep_max :: {Int} -> {Int}
keep_max arr 
| length (toList arr) == 1 = {}
= {x\\x<-: arr | maxList (toList arr) == x}
//Start = keep_max {1,4,5,3,3,2,4,5,1,3,4} // {5,5}
//Start = keep_max {1,42,42,52,452,4} // {452}
//Start = keep_max {1,6,6,6,6} // {6,6,6,6}
//Start = keep_max {5} // {}
//Start = keep_max {} // {}
/*works for all */

/*
	5. Given two Strings as parameters, remove all characters of second string from the first one.
	This task is not case sensitive, so upper and lower case letters count as same. Ex.: 'A' == 'a'.
	Hint: toLower function can be used in this function.
*/
/*
isRem :: Char String -> Bool
isRem c s = length[1 \\ x <-: s | c == x] == 0
//    Z s2         1    z     z   Z == z  
remove_from_first_string :: String String -> String
remove_from_first_string s1 s2 = {x \\ x <-: s1 | isRem x s2}
*/
remove_from_first_string :: String String -> String
remove_from_first_string a1 b1 = {x\\x<-: a1 | not(isMember x [y\\y<-: b1])}
//Start = remove_from_first_string "Zuka" "z"// "Zuka"
//Start = remove_from_first_string "XccEcxacXmXs aXcrccXe hXaXccXbrXd" "Xbc"// "Exams are hard"
//Start = remove_from_first_string "Clean is the best" " "// "Cleanisthebest"
//Start = remove_from_first_string "It's a nice weather outside" ""// "It's a nice weather outside"
//Start = remove_from_first_string "" ""// ""

/*
	6. Given a predefined Shape type, argument of the Circle constructor is the radius,
    side length for Square, and equilateral Triangle, width and height for Rectangle.
    write a function that calculates the area and circumference (perimeter) of each shape
    and returns one that has maximum difference between those two.
     			Circumference		Area
    Circle			2*r*pi			r^2*pi		p=3.14
    Square			4*a				a^2
    Tiangle			3*a				sqrt(3)*a^2/4
    Rectangle		2*a+2*b			a*b
    Use pi = 3.1415926535897932384
*/

:: Shape = Circle Real
        |  Square Real
        |  Triangle Real
        |  Rectangle Real Real
       
area :: Shape -> Real
area (Circle r) = 3.14*r^2.0
area (Square a) = a*a
area (Triangle b) = (sqrt(3.0)*b^(2.0))/4.0
area (Rectangle c d) = c*d
//Start = area (Rectangle 2.0 4.0)
//Start = area (Circle 3.0) //28.26  
//Start = area (Square 2.5) // 6.25
//Start = area (Triangle 7.6)
circumference :: Shape -> Real
circumference (Circle r) = 2.0*r*3.14
circumference (Square a) = 4.0*a
circumference (Triangle b) = 3.0*b
circumference (Rectangle c d) = 2.0*(c+d)
//Start = circumference (Rectangle 2.0 4.0)
//Start = circumference (Circle 1.75) //10.99
//Start = circumference (Square 0.95) // 3.8
//Start = circumference (Triangle 7.6) // 22.8
isMax :: Real [Real] -> Bool
isMax n [x:xs]
| maxList [x:xs] == n = True
= False
toList1 :: {Shape} -> [Shape]
toList1 arr = [x\\x<-: arr]
max_area_perimeter_diff :: {Shape} -> (Real, Real)
max_area_perimeter_diff shapes =  ( hd[ ((area x),(circumference x))\\x <-(toList1 shapes) | isMax (abs((area x)-(circumference x))) [ abs((area x)-(circumference x))\\x <- (toList1 shapes)]] )

//Start = max_area_perimeter_diff {(Circle 3.0), (Square 2.5)} // (28.26, 18.84)
//Start = max_area_perimeter_diff {(Triangle 4.3), (Rectangle 5.4 7.2), (Circle 2.45)} // (38.88, 25.2)
//Start = max_area_perimeter_diff {(Triangle 7.6), (Circle 1.75), (Square 0.95)} // (0.9025, 3.8)
/*works for all */

/*
	7. A coach was choosing the starting team for his important game, unfortunately, he/she has an issue 
        choosing the starting striker, let's help to choose, the best striker is a striker who has the
	best average in scoring goals. There is another factor which the moral if the player is happy, this
	adds 0.1 to his average. And of course, we should choose the striker with the best average. (just return the name)
	NOTE: If there are 2 players with the same average (after considiring the moral of course) return any of them.
*/

:: Moral = Happy | Sad
:: Player = {player_name :: String, goals :: [Int], moral :: Moral}

instance == Moral
where
	(==) Happy Happy = True
	(==) Sad Sad = True
	(==) _ _ = False

instance < Player
where
	(<) a b = (<) (getAverage a) (getAverage b)

getAverage :: Player -> Real
getAverage p
| p.moral == Happy = (avg goals_real) + 0.1
= avg goals_real
where
	goals_real = [toReal x \\ x <- p.goals]

best_striker :: [Player] -> String
best_striker players = (maxList players).player_name
//Start = best_striker [{player_name = "Abo Trika", goals = [1,1,0,0,2], moral = Happy}, {player_name = "Ronaldo", goals = [1,1,2,0,2], moral = Sad}, {player_name = "Messi", goals = [1,1,2,1,2,2,0], moral = Happy} ] // Messi
//Start = best_striker [{player_name = "Abo Trika", goals = [2,2], moral = Sad}, {player_name = "Ronaldo", goals = [2,2], moral = Happy}, {player_name = "Messi", goals = [1,1,2,1,2,2,0], moral = Sad} ] //Ronaldo

/*
	8. In a boxing match, there are two fighters and we need to run a simulations to determine who will win
	(return the winner's name and the number of round he won at in a tuple).
	Each fighter has: health points, attacking points and defense points.
	The match is 10 rounds,
	in each round one of the two fighters can punch the other fighter and the round ends, and in the next round the other fighter
	will be the one who throw the punch and so on.
	The punch should subtract x points from the health points of the fighter (who gets hit of course), x = attacking points - defending points.
	In case fighter's health points reaches 0, he/she loses.
	NOTE: The first fighter (first argument in the function) always starts.
	NOTE: If 10 rounds passed without any winner return ("No winner", 22)
	NOTE: If the defending points is bigger than the attacking points of the other fighter the health points stay the same
	Example:
	Fighter A has health points: 100, attacking points: 50, defending points: 30.
	Fighter B has health points: 40, attacking points: 20, defending points: 10.
	when A hits B, B's health points will be equal to 40 - (50 - 10) = 0.
*/


:: Fighter = {fighter_name :: String, health_points :: Int, attacking_points :: Int, defending_points :: Int}

rumble_in_the_jungle1 = ({fighter_name = "Ali", health_points = 100, attacking_points = 30, defending_points = 40}, {fighter_name = "Foreman", health_points = 90, attacking_points = 50, defending_points = 20})
rumble_in_the_jungle2 = ({fighter_name = "Ali", health_points = 100, attacking_points = 70, defending_points = 40}, {fighter_name = "Foreman", health_points = 100, attacking_points = 50, defending_points = 20})
rumble_in_the_jungle3 = ({fighter_name = "Ali", health_points = 50, attacking_points = 70, defending_points = 40}, {fighter_name = "Foreman", health_points = 150, attacking_points = 80, defending_points = 20})
thrilla_in_manila = ({fighter_name = "Frazier", health_points = 50, attacking_points = 70, defending_points = 40}, {fighter_name = "Ali", health_points = 150, attacking_points = 80, defending_points = 20})

getWinnerRoundAux :: Fighter Fighter Int -> (String, Int)
getWinnerRoundAux a b c
| a.health_points <= 0 = (b.fighter_name, c-1)
| c == 11 = ("No winner", 22)
| b.defending_points > a.attacking_points = getWinnerRoundAux b a (c+1)
= getWinnerRoundAux {b & health_points = (b.health_points - (a.attacking_points - b.defending_points))} a (c+1)

getWinnerRound :: Fighter Fighter -> (String, Int)
getWinnerRound a b = getWinnerRoundAux a b 1

//Start = getWinnerRound {fighter_name = "Ali", health_points = 100, attacking_points = 30, defending_points = 40} {fighter_name = "Tayson", health_points = 90, attacking_points = 50, defending_points = 20} // ("No winner", 22)
// Start = getWinnerRound {fighter_name = "Ali", health_points = 100, attacking_points = 70, defending_points = 40} {fighter_name = "Tayson", health_points = 100, attacking_points = 50, defending_points = 20} // ("Ali",3)
// Start = getWinnerRound {fighter_name = "Ali", health_points = 50, attacking_points = 70, defending_points = 40} {fighter_name = "Tayson", health_points = 150, attacking_points = 80, defending_points = 20} // ("Tayson",4)

/*
fi :: (Fighter, Fighter) Int -> (String, Int)
fi (a, b) c
|a.health_points == 0 = (b.fighter_name, 0)
|b.health_points == 0 = (a.fighter_name, 0)
| isEven c = b.health_points = b.health_points - (a.health_points - b.defending_points)
fight :: (Fighter, Fighter) -> (String, Int)
fight (a, b) = b.health_points = b.health_points - (a.health_points - b.defending_points)
| a.health_points == 0 = (b.fighter_name, )
This is just trash */

/*
	9. Write a filter function for colored rose tree.
	Colored rose Tree is a tree where each node has
	some value, color and children nodes stored in list.
	Your filter function should take tree, color and a
	condition function as an argument. Return a list of
	values stored in nodes which have given color and
	satisfy given condition (Condition function returns
	true for node's value)
*/

:: NodeColor = Red | Green | Blue
:: ColoredRoseTree a = Node a NodeColor [ColoredRoseTree a] | Leaf

tree1 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Leaf])]
tree2 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Node 7 Red [Node 9 Red [], Node 10 Red []]])]

instance == NodeColor
where
    (==) Red Red = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) _ _ = False

filterColoredTree :: (ColoredRoseTree a) NodeColor (a -> Bool) -> [a]
filterColoredTree Leaf  _ _ = []
filterColoredTree (Node x c children) col pred = curr ++ rans
where
    rans = flatten [filterColoredTree child col pred \\ child <- children]
    curr | (pred x) && (c == col) = [x] = []

// Start = filterColoredTree tree1 Blue isEven // [2,4]
//Start = filterColoredTree tree1 Blue isOdd // [3]
//Start = filterColoredTree tree2 Red (\x = True) // [1.7,9,10]
//Start::[Int] // Uncomment this line too, to run next test
//Start = filterColoredTree Leaf Green isOdd // []

/*
	10. You are given a family tree. Each Node stores
	Person and his/her parent's family trees. If
	the data about the person is not known than the
	node is 'Unknown'. Your task is to count how many
	person are related to ELTE. A Person is related to
	ELTE if his/her father or mother studied at ELTE.
*/
::Gender = Male | Female
::Uni = ELTE | BME | MIT | TUM | LMU



::Person = { name :: String
           , gender :: Gender
           , almaMater :: Uni
           }


::FamilyTree = Member Person FamilyTree FamilyTree | Unknown


p1 = {name="A",gender=Male,almaMater=ELTE}
p2 = {name="B",gender=Female,almaMater=TUM}
p3 = {name="C",gender=Male,almaMater=BME}
p4 = {name="D",gender=Female,almaMater=ELTE}
ftree1 = Member p2 (Member p1 (Member p3 Unknown Unknown) Unknown) (Member p4 (Member p2 Unknown Unknown) (Member p4 Unknown Unknown))
ftree2 = Member p3 ftree1 (Member p3 (Member p1 Unknown ftree1) (Member p4 ftree1 ftree1))
ftree3 = Member p1 (Member p3 ftree2 (Member p4 ftree1 ftree2)) (Member p3 Unknown (Member p4 (Member p3 ftree2 Unknown) ftree2))


instance == Uni
where
    (==) ELTE ELTE = True
    (==) _ _ = False


countRelated :: FamilyTree -> Int
countRelated Unknown = 0
countRelated (Member p par1 par2) = curr + (countRelated par1) + (countRelated par2)
where
    curr | isRelated (Member p par1 par2) = 1 = 0

isRelated :: FamilyTree -> Bool
isRelated Unknown = False
isRelated (Member p par1 par2) = studiedAtELTE par1 || studiedAtELTE par2

studiedAtELTE :: FamilyTree -> Bool
studiedAtELTE Unknown = False
studiedAtELTE (Member p par1 par2) = p.almaMater == ELTE

// Start = countRelated ftree1 // 2
// Start = countRelated ftree2 // 9
// Start = countRelated Unknown // 0
// Start = countRelated ftree3 // 40








