module p10
import StdEnv


/*
Write binary tree ADT. It should have two types: Node with value and left and right subtrees and Leaf.
Write inOrderTraversalMapping function which takes two arguments mapping function and tree.
It should return mapped values from given tree in inorder. Inorder list is a list
of values from the tree, where each node's value is in the middle of its children. First comes
nodes from the left subtree, then the node itself and at the end nodes from the right subtree.

Ex.
Tree: 1
/ \
2 3
/\
4 5
Inorder list: [2, 1, 4, 3, 5]
If Mapping function is (\x = 10*x + 1) then final result is:
[21, 11, 41, 31, 51]
*/

// TODO
:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))

tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))

tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))

treeList :: (Tree a) -> [a]
treeList Leaf = []
treeList (Node x l r) = (treeList l) ++ [x] ++ (treeList r)
//Start = treeList minRoot


// TODO

/*
isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

inOrderTraversalMapping:: (a -> b) (Tree a) -> [b]
inOrderTraversalMapping func (Node x l r) 
| isLeaf (Node x l r) = []
= [func a\\a<- (treeToList (Node x l r))]

treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) = (treeToList l) ++ [x] ++ (treeToList r)
*/

Start = inOrderTraversalMapping (\x = 2*x+1) tree1 // [13,21,23,9,25,41]
//Start = inOrderTraversalMapping (\x = (toReal x) * 2.5) tree2 // [2.5,77.5,25,12.5,30,35,77.5,42.5,27.5]
// Start = inOrderTraversalMapping (\x = "X" ) tree3 // ["X","X","X","X","X","X","X","X","X","X"]
// Start = inOrderTraversalMapping (\x = toInt ((toReal x) / 3.0)) tree3 // [11,4,4,4,2,2,6,1,1,1

treeList :: (Tree a) -> [a]
treeList Leaf = []
treeList (Node x l r) = (treeList l) ++ [x] ++ (treeList r)
//Start = treeList minRoot


// TODO
inOrderTraversalMapping:: (a -> b) (Tree a) -> [b]
inOrderTraversalMapping _ Leaf = []
inOrderTraversalMapping maps tree = map maps (treeList tree)




