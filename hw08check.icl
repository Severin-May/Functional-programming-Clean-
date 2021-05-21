module hw08check 
import StdEnv

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

isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

subTreeList :: (Tree a) -> [Tree a]
subTreeList Leaf = []
subTreeList tree = subTreeList (goL tree) ++ [tree] ++ subTreeList (goR tree)
//Start = subTreeList tree1

goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l

goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r

countLeaves :: (Tree a) -> Int
countLeaves Leaf = 1
countLeaves (Node x Leaf r) = 1 + countLeaves r
countLeaves (Node x l Leaf) = 1 + countLeaves l
countLeaves (Node x l r) = (countLeaves l) + (countLeaves r)
//Start = countLeaves tree1 // 7
//Start = countLeaves tree2 // 10
//Start = countLeaves tree3 // 11
//Start = countLeaves tree4 // 38
//Start = countLeaves tree5 // 49
/* works for all */

/* 2. Given the binary tree, find how many nodes are there such that they have exactly
3 grandchildren non-leaf nodes.
   1
  / \
  2 3
/ \ / \
4 5 6 Leaf
1st node has exactly 3 grandchildrens, so it's a 'good' node.
*/


extractNode :: (Tree a) -> a
extractNode (Node x l r) = x

getChildren :: a (Tree a) -> [a] | Eq a
getChildren n tree
| isLeaf(goL subtree)&&isLeaf(goR subtree)=[]
| isLeaf(goL subtree) = [extractNode(goR subtree)]
| isLeaf(goR subtree) = [extractNode(goL subtree)]
= [extractNode(goL subtree)]++[extractNode(goR subtree)]
    where
        subtree = hd(extractSubLists n tree)
//Start = (getChildren 10 (goL tree1)) ++ (getChildren 20 tree1)

countTripleParents :: (Tree Int) -> Int
countTripleParents Leaf = 0
countTripleParents (Node x Leaf Leaf) = 0
countTripleParents (Node x l r) 
|isLeaf l = countTripleParents r
|isLeaf r = countTripleParents l
| ( length(getChildren (extractNode l) l) + length(getChildren (extractNode r) r)) == 3 = 1 + countTripleParents l + countTripleParents r
= countTripleParents l + countTripleParents r

Start = countTripleParents tree1 // 1 
//Start = countTripleParents tree2 // 1
//Start = countTripleParents tree3 // 1
//Start = countTripleParents tree4 // 4
//Start = countTripleParents tree5 // 5
/* works for all */


/* 3. Implement a function that interleaves three arrays. So for input arrays {1,2,3}, {4,5,6}
and {7,8,9} the function must return the array {1,4,7,2,5,8,3,6,9}. If an array is out of elements
we continue interleaving the remaining arrays.
Example: {1,2} {3,4,5,6} {7,8,9} -> {1,3,7,2,4,8,5,9,6}
*/

/*
w [][b:bs][c:cs] = flatten[[b,c]\\b<-[b:bs] & c<-[c:cs]]
w [a:as][][c:cs] = flatten[[a,c]\\a<-[a:as] & c<-[c:cs]]
w [a:as][b:bs][] = flatten[[a,b]\\a<-[a:as] & b<-[b:bs]]
w [a:as][b:bs][c:cs] = flatten[[a,b,c]\\a<-[a:as] & b<-[b:bs] & c<-[c:cs]]
*/

w :: [Int][Int][Int] -> [Int]
w [] [] [] = []
w [][b:bs][c:cs] = [b,c] ++ w [] bs cs
w [] [] [c:cs] = [c] ++ w [] [] cs
w [] [b:bs] [] = [b] ++ w [] bs []
w [a:as] [] [] = [a] ++ w as [] []
w [a:as][][c:cs] = [a,c] ++ w as [] cs
w [a:as][b:bs][] = [a,b] ++ w as bs []
w [a:as][b:bs][c:cs] = [a,b,c] ++ w as bs cs
//Start = w [1,2,3][4,5,6][7,8,9]
//Start = w [1,2] [3,4,5,6] [7,8,9]
//Start = w [1,2] [3,4,5] [6,7,8,9,10,11,12]

toList :: {Int} -> [Int] 
toList arr = [x\\x<-:arr]

interleave :: {Int} {Int} {Int} -> {Int}
interleave a1 a2 a3 = {x\\x<- (w (toList a1) (toList a2) (toList a3))}

//Start = interleave {1,2,3} {4,5,6} {7,8,9} // {1,4,7,2,5,8,3,6,9}
//Start = interleave {1,2} {3,4,5,6} {7,8,9} // {1,3,7,2,4,8,5,9,6}
//Start = interleave {} {1,2,3} {4} // {1,4,2,3}
//Start = interleave {} {} {} // {}
//Start = interleave {1,2} {3,4,5} {6,7,8,9,10,11,12} // {1,3,6,2,4,7,5,8,9,10,11,12}
/* works for all */

/*not needed for hw
counter :: (Tree Int) -> Bool
counter Leaf = False
counter (Node x Leaf Leaf) = False
counter (Node x l r)
| not(isLeaf (goL l)) && not(isLeaf (goR l)) && ( not(isLeaf (goL r)) || not(isLeaf (goR r)) ) = True
| not(isLeaf (goL r)) && not(isLeaf (goR r)) && ( not(isLeaf (goL l)) || not(isLeaf (goR l)) ) = True
| isLeaf l = counter r
| isLeaf r = counter l
= False
*/
//Start = counter tree4

