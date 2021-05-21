module hw8
import StdEnv



// Start = ourTree
// extract Node

extractNode :: (Tree a) -> a
extractNode (Node x l r) = x

// Start = extractNode Leaf

goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l

goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r

// Start = goL ourTree
// Start = goR ourTree

isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False
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

/* 1. Given the binary tree, count number of leaf nodes in the tree.
*/

countLeaves :: (Tree a) -> Int
countLeaves (Node x l r ) = cl (Node x l r ) 0

cl :: (Tree a) Int -> Int
//cl Leaf c = c+2
cl (Node x l r) c
|(isLeaf l == False) && (isLeaf r == True) = cl l (c+1)
|(isLeaf l ==True) && (isLeaf r == False) = cl r (c+1)
| (isLeaf l ==False) && (isLeaf r == False) =(cl l c) + (cl r c)  
=c+2
Start = countLeaves tree1 // 7
//Start = countLeaves tree2 // 10
//Start = countLeaves tree3 // 11
//Start = countLeaves tree4 // 38
//Start = countLeaves tree5 // 49

/* 2. Given the binary tree, find how many nodes are there such that they have exactly
3 grandchildren non-leaf nodes.
Ex.: 
 1
/  \
2   3
/ \ / \
4 5 6 Leaf
1st node has exactly 3 grandchildrens, so it's a 'good' node.
*/

subTreeList :: (Tree a) -> [(Tree a)]
subTreeList Leaf = []
subTreeList tree = subTreeList(goL tree) ++ [tree] ++ subTreeList(goR tree)

//Extract sublists countaining a specific element
extractSubLists :: a (Tree a) -> [(Tree a)] | Eq a
extractSubLists n tree = [subtree\\subtree<-(subTreeList tree)|(extractNode subtree)==n]
//Start = extractSubLists 10 tree1

getChildren :: a (Tree a) -> [a] | Eq a
getChildren n tree
| isLeaf(goL subtree)&&isLeaf(goR subtree)=[]
| isLeaf(goL subtree) = [extractNode(goR subtree)]
| isLeaf(goR subtree) = [extractNode(goL subtree)]
= [extractNode(goL subtree)]++[extractNode(goR subtree)]
    where
        subtree = hd(extractSubLists n tree)
//Start = getChildren 4 tree1

getGrandChildren :: a (Tree a) -> [a] | Eq a
getGrandChildren _ Leaf = []
getGrandChildren _ (Node x Leaf Leaf) = []
getGrandChildren n (Node x Leaf r) = (getChildren (extractNode r) r)
getGrandChildren n (Node x l Leaf) = (getChildren (extractNode l) l)
getGrandChildren n (Node x l r) = (getChildren (extractNode l) l) ++ (getChildren (extractNode r) r)
//Start = getGrandChildren 4 tree1
 
countTripleParents :: (Tree Int) -> Int
countTripleParents Leaf = 0
countTripleParents (Node x l r )
| length(getGrandChildren x (Node x l r)) == 3 = 1 + countTripleParents l + countTripleParents r
= countTripleParents l + countTripleParents r

//Start = countTripleParents tree1 // 1
//Start = countTripleParents tree2 // 1
//Start = countTripleParents tree3 // 1
//Start = countTripleParents tree4 // 4
//Start = countTripleParents tree5 // 5

/* 3. Implement a function that interleaves three arrays. So for input arrays {1,2,3}, {4,5,6}
and {7,8,9} the function must return the array {1,4,7,2,5,8,3,6,9}. If an array is out of elements
we continue interleaving the remaining arrays.
Example: {1,2} {3,4,5,6} {7,8,9} -> {1,3,7,2,4,8,5,9,6}
*/
ToList:: {Int} -> [Int]
ToList arr = [x\\x<-: arr]
ToArr:: [Int] -> {Int}
ToArr list = {x\\x<- list}

interleave :: {Int} {Int} {Int} -> {Int}
interleave a b c =    ToArr (il (ToList a) (ToList b) (ToList c)) 
//isEmpty 

il [] [] [] = []
il [] [] c = c
il c [] [] = c
il [] c [] =c
il [] a b = il2 a b
il a [] b = il2 a b
il a b [] = il2 a b
il [a:b] [c:d] [x:y] = [a,c,x : (il b d y)]

il2 [] [] = []
il2 [] a = a
il2 a [] = a

il2 [a: b] [c:d] =[a,c : (il2 b d)] 

//Start = interleave {1,2,3} {4,5,6} {7,8,9} // {1,4,7,2,5,8,3,6,9}
//Start = interleave {1,2} {3,4,5,6} {7,8,9} // {1,3,7,2,4,8,5,9,6}
//Start = interleave {} {1,2,3} {4} // {1,4,2,3}
//Start = interleave {} {} {} // {}
//Start = interleave {1,2} {3,4,5} {6,7,8,9,10,11,12} // {1,3,6,2,4,7,5,8,9,10,11,12}






















