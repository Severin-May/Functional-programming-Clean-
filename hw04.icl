module hw04
import StdEnv

/*
1.Given two numbers L and R and the list of tuples, 
where the first element is an number
and the second is string. Return the list of 
strings from tuples where first element
is a number in interval [L,R].
*/

stringInRange :: Int Int [(Int,String)] -> [String]
stringInRange n m [x:xs] = [(snd y)\\y <- [x:xs] | (fst y)>= n && (fst y) <= m]
//Start = stringInRange 2 7 [(1,"A"), (2,"B"), (3,"C"), (11,"D"), (3,"E"), (5,"F")] // ["B","C","E","F"]
//Start = stringInRange 1 15 [(1,"A"), (2,"B"), (3,"C"), (11,"D"), (3,"E"), (5,"F")] // ["A","B","C","D","E","F"]
//Start = stringInRange 2 3 [(1,"A"), (2,"B"), (5,"F")] // ["B"]
//Start = stringInRange 3 4 [(1,"A"), (2,"B"), (5,"F")] // []
//Start = stringInRange 7 2 [(1,"A"), (2,"B"), (5,"F"), (3,"C"), (11,"D"), (3,"E"), (5,"F")] // []
//works for all cases


/*
2.Write a function which takes 2 list of integers and 1 list of booleans and returns
a single list of tuples, where i-th tuple contains i-th elements from input lists.
Returned list's size should be equal to the shortest input list's size.
*/

tripleZip :: [Int] [Int] [Bool] -> [(Int,Int,Bool)]
tripleZip [] _ _ = []
tripleZip _ [] _ = []
tripleZip _ _ [] = []
tripleZip intL1 intL2 boolL = [(x,y,z)\\x<-intL1 & y <- intL2 & z <- boolL]

//Start = tripleZip [1,5,7] [4,10,2,3] [False, True, True] // [(1,4,False),(5,10,True),(7,2,True)]
//Start = tripleZip [1..10] [11..20] [False \\ x <- [1..50]] // [(1,11,False),(2,12,False),(3,13,False),(4,14,False),(5,15,False),(6,16,False),(7,17,False),(8,18,False),(9,19,False),(10,20,False)]
//Start = tripleZip [1,3..20] [4,10,2,3] [False, True, True] // [(1,4,False),(3,10,True),(5,2,True)]
//Start = tripleZip [] [11..20] [True \\ x <- [1..15]] // []
//works for all cases

/*
3.Write a function which gets a number K and list of numbers and divides it into sublists
of size K. Last partition may have smaller size than K. After that sort each sublist and
return list of lists.
Ex.: K = 3, List = [1,7,3,5,2,3,11,8] -> [[1,7,3], [5,2,3], [11,8]]
-> [[1,3,7],[2,3,5],[8,11]]
*/

listDivider :: Int [Int] -> [[Int]]
listDivider _ [] = []
listDivider k list = [sort(fst (splitAt k list))] ++ listDivider k (snd (splitAt k list))
//[fst(splitAt k (snd (splitAt k list)))]


//Start = listDivider 3 [1,7,3,5,2,3,11,8] // [[1,3,7],[2,3,5],[8,11]]
//Start = listDivider 1 [5,4..2] // [[5],[4],[3],[2]]
//Start = listDivider 4 [5,4..2] // [[2,3,4,5]]
//Start = listDivider 5 ([7,6..2] ++ [1..5] ++ [5,7..11]) // [[3,4,5,6,7],[1,2,2,3,4],[5,5,7,9,11]]
//works for all cases





