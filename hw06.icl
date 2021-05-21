module hw06
import StdEnv

/*1. Given two sorted lists of unique integers, return lists which contains their union.
Union of the two lists should contain all integers which appear in at least one of them.
Each element should be added only once. Elements in union should be sorted in ascending order.
Ex.: [1,2,3,4,5] [4,5,6,7] -> [1,2,3,4,5,6,7]
*/

listUnion :: [Int] [Int]-> [Int]
listUnion [] [] = []
listUnion lst1 lst2 = sort(removeDup(lst1 ++ lst2))

//Start = listUnion [1,3,4,7,8,12,13] [1,4,6,10,11,12,15] // [1,3,4,6,7,8,10,11,12,13,15]
//Start = listUnion [1..5] [1,3..10] // [1,2,3,4,5,7,9]
//Start = listUnion [1,2] [] // [1,2]
//Start = listUnion [] [] // []
//works for all cases

/*2. Given the list of points and a distance. Each point is represented with tuple, containing 
X and Y coordinates in 2D plane. Return how many pair of points are there so that, distance 
between them is equal to the given value.
d=v((x_2-x_1)²+(y_2-y_1)²)
*/

dist :: (Int, Int) [(Int, Int)] -> [Real]
dist _ [] = []
dist p [x:xs] = [( sqrt( ((toReal (fst x)) - (toReal (fst p)))^2.0 + ((toReal (snd x)) - (toReal (snd p)))^2.0 ) )] ++ dist p xs
//| (fst p) <> (fst x) && (snd p) <> (snd x) = [( sqrt( ((toReal (fst x)) - (toReal (fst p)))^2.0 + ((toReal (snd x)) - (toReal (snd p)))^2.0 ) )] ++ dist p xs
//= dist p xs
//Start = dist (1,1) [(1,1), (4,5), (8,8), (10,3)] 
//Start = dist (3,4) [(3,4), (3,8), (7,8)]

dist1 :: [(Int, Int)]  -> [[Real]]
dist1 [] = []
dist1 [x:xs] = [dist x [x:xs]] ++ dist1 xs
//Start = dist1 [(1,1), (4,5), (8,8), (10,3)] 
//Start = dist1 [(3,4), (3,8), (7,8)]


pointDistance :: [(Int,Int)] Int -> Int
pointDistance [] _ = 0
pointDistance pts input = sum[1\\ b <- flatten(dist1 pts) | b == toReal input]

//pointDistance [x:xs] input = [(distance x y)\\ x<- [x:xs]& y <- [x:xs] | (distance x y) == input && (fst x) <> (fst y) && (snd x) <> (snd y)]
//Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 5 // 2
//Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 2 // 0
//Start = pointDistance [(3,4), (3,8), (7,8)] 4 // 2
//Start = pointDistance [] 3 // 0
//Start = pointDistance [(1,1)] 2 // 0
//works for all cases


/*3. Given the list of integers, modify it in a following way:
I. Remove all numbers which are multiple of 3
II. Sort remaining list in descending order
III. Swap 1st and 2nd elements, 3rd and 4th, 5th and 6th and so on.
*/
remove3 :: [Int] -> [Int] 
remove3 [] = []
remove3 list = [x\\ x<- list | x rem 3 <> 0]

swap :: [Int] -> [Int]
swap [] = []
swap [x] = [x]
swap [x,y:xs] = [y,x] ++ swap xs
//Start = swap [1,2,3,4,5]

//(reverse(sort(remove3 [x:xs])))

shuffleSort :: [Int] -> [Int]
shuffleSort [] = []
shuffleSort [x] = [x]
shuffleSort lst = [x\\ x <- swap (reverse(sort(remove3 lst)))]

//Start = shuffleSort [4,3,2] // [2,4]
//Start = shuffleSort [4,1,3,2,5,6,7] // [5,7,2,4,1]
//Start = shuffleSort [3,6,3,9,12] // []
//Start = shuffleSort [2,4,5,7,14,17] // [14,17,5,7,2,4]
//Start = shuffleSort [] // []
//works for all cases







