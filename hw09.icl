module hw09
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

minRoot :: Tree Int
minRoot = (Node 4 (Node 10 (Node 11 Leaf Leaf)(Node 16 Leaf Leaf)) (Node 22 (Node 15 Leaf Leaf) Leaf))

minMostLeftLeaf :: Tree Int
minMostLeftLeaf = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 22 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf Leaf) ))

minMostRightLeaf :: Tree Int
minMostRightLeaf = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 22 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf (Node 1 Leaf Leaf)) ))

minNode :: Tree Int
minNode = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node -12 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf Leaf) ))

/*
1. Given a Tree of integers, find the minimum value of all the nodes.
*/
treeList :: (Tree Int) -> [Int]
treeList Leaf = []
treeList (Node x l r) = (treeList l) ++ [x] ++ (treeList r)
//Start = treeList minRoot

minTree :: (Tree Int) -> Int
minTree Leaf = abort "The tree is empty!"
minTree tree = minList (treeList tree)

//Start = minTree minRoot // 4
//Start = minTree minMostLeftLeaf //2
//Start = minTree minMostRightLeaf //1
//Start = minTree minNode // -12


:: Major = CS | Math | Physics
:: Course = {name::String, major :: Major, credits:: Int}

Programming::Course
Programming = {name="Programming",major=CS, credits =5}
Analysis::Course
Analysis = {name="Analysis",major=Math, credits =4}
Relativity::Course
Relativity = {name="Relativity",major=Physics,credits=6}
Functional::Course
Functional = {name="Functional",major=CS,credits=5}
Basic::Course
Basic = {name="Basic",major=Math,credits=3}
Thermo_Dynamics::Course
Thermo_Dynamics = {name="Thermo_Dynamics",major=Physics,credits=4}
Astronomy::Course
Astronomy = {name="Astronomy",major=Physics,credits=6}
Numerical_Methods::Course
Numerical_Methods = {name="Numerical_Methods",major=Math,credits=4}
Compilers::Course
Compilers = {name="Compilers",major=CS,credits=4}



/*
2. Given a list of Courses that a student has taken,
find the credits they earned if
for each CS major course, he gets 3 more credits
*/

instance == Major
where
	(==) CS CS = True
	(==) Physics Physics = True
	(==) _ _ = False

creds :: [Course] -> Int
creds [] = 0
creds [x:xs] = sum([x.credits+3\\x<- [x:xs] | x.major == CS]) + sum([x.credits\\x<- [x:xs] | not(x.major == CS)])

//Start = creds [Compilers, Astronomy, Basic] // 16
//Start = creds [Thermo_Dynamics, Relativity, Numerical_Methods] // 14
//Start = creds [Compilers, Functional, Programming] //23
//Start = creds [] // 0
//works for all

//:: Tree a = Node a (Tree a) (Tree a ) | Leaf

treea :: Tree Course
treea = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) (Node Thermo_Dynamics Leaf Leaf)) Leaf) (Node Basic (Node Compilers (Node Relativity Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))
treeb :: Tree Course
treeb = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) Leaf) Leaf)(Node Relativity (Node Compilers (Node Numerical_Methods Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))
treec :: Tree Course
treec = ( Node Analysis Leaf ( Node Programming Leaf ( Node Astronomy Leaf ( Node Basic Leaf ( Node Compilers Leaf( Node Thermo_Dynamics Leaf ( Node Numerical_Methods Leaf (  Node Functional Leaf Leaf) ) ) ) ) ) ) )

/*
3. Given a tree of Courses, give back all the CS Courses
whose both children are either Physics courses or Leaf-s.
*/

/*
instance == Major
where
	(==) Physics Physics = True
	(==) CS CS = True
	(==) _ _ = False
*/

isLeaf :: (Tree Course) -> Bool
isLeaf Leaf = True
isLeaf _ = False

extractNode :: (Tree Course) -> Course
//extractNode (Node x Leaf Leaf) = x
extractNode (Node x l r) = x


isTreeLeaf :: (Tree Course) -> Bool
isTreeLeaf Leaf = False
isTreeLeaf (Node x l r)
| ((isLeaf l) && (isLeaf r) && x.major == CS) ||  ( ( (extractNode l).major == Physics) && ((extractNode r).major == Physics) && x.major == CS) || ( ((isLeaf l) && ((extractNode r).major == Physics) && x.major == CS )  || ( ((isLeaf r) && ((extractNode l).major == Physics) && x.major == CS))) = True
= False
//Start = isTreeLeaf treea

getphysics :: (Tree Course) -> [Course]
getphysics Leaf = []
getphysics (Node x l r)
| isTreeLeaf (Node x l r) = [x] ++ getphysics l ++ getphysics r
| isLeaf l && isTreeLeaf (Node x l r) = [x] ++ getphysics r
| isLeaf r && isTreeLeaf (Node x l r) = [x] ++ getphysics l
| isLeaf l && isTreeLeaf (Node x l r) && isLeaf r = [x]
= getphysics l ++ getphysics r

//Start = getphysics treea // [(Course "Programming" CS 5),(Course "Compilers" CS 4)]
//Start = getphysics treeb // [(Course "Functional" CS 5),(Course "Programming" CS 5)]
//Start = getphysics treec // [(Course "Programming" CS 5),(Course "Compilers" CS 4),(Course "Functional" CS 5)]
//partly works. I could not handle the runtime error in extractNode









//| ((isLeaf l) && (isLeaf r) && x.major == CS) ||  ( ( (extractNode l).major == Physics) && ((extractNode r).major == Physics) && x.major == CS) || ( ((isLeaf l) && ((extractNode r).major == Physics) && x.major == CS )  || ( ((isLeaf r) && ((extractNode l).major == Physics) && x.major == CS))) = [x] ++ getphysics l ++ getphysics r
//| (x.major == CS && isLeaf l && isLeaf r) || (x.major == CS && (extractNode l).major == Physics && (extractNode r).major == Physics) = [x]

/*
getChildrenAux (Node x Leaf Leaf) = []
getChildrenAux (Node x l r) = [ (extractNode l), (extractNode r)]
//Start = getChildrenAux treeb

isit :: (Tree Course) -> Bool
isit Leaf = False
isit (Node x l r)
| (and[b.major == Physics\\b<-(getChildrenAux (Node x l r))] && x.major == CS) || (and[isLeaf b\\b<-(getChildrenAux (Node x l r))] && x.major == CS) = True
|( isLeaf( ( getChildrenAux (Node x l r) )!!0 ) && ((((getChildrenAux (Node x l r))!!1).major == Physics) && x.major == CS) || ( isLeaf( ( getChildrenAux (Node x l r) )!!1 ) && ( ( ( getChildrenAux (Node x l r) )!!0 ).major == Physics ) && x.major == CS)) = True
= False
//Start = isit treea
*/

/*
leafPhysList :: [(Tree Course)] -> [Course]
leafPhysList [] = []
leafPhysList lst = [extractNode c\\c<- (lst) | (c.major == CS && (isLeaf (goL c)) && (isLeaf (goR c))) ||( c.major == CS && (((extractNode (goL c)).major == Physics) && ((extractNode (goR c)).major == Physics)) )]
Start = leafPhysList (subTreeList treea)
*/






















