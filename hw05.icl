module hw05
import StdEnv

/*
1. Write a function which takes a list of numbers and returns tuple
of two lists: I. Even numbers from the list sorted in ascending order.
II. Odd numbers from the list sorted in descending order.
Ex. [1,3,2,7,12,4,1,4,5,12,9,4] -> [2,4,4,4,12,12] [9,7,5,3,1,1]
*/

splitSort :: [Int] -> ([Int],[Int])
splitSort [] = ([],[])
splitSort list = ((sort[x\\x<-list | isEven x]),(reverse(sort[x\\x<- list | isOdd x])))

//Start = splitSort [1,3,2,7,12,4,1,4,5,12,9,4] // ([2,4,4,4,12,12],[9,7,5,3,1,1])
//Start = splitSort [1,2..13] // ([2,4,6,8,10,12],[13,11,9,7,5,3,1])
//Start = splitSort [2,4..14] // ([2,4,6,8,10,12,14],[])
//Start = splitSort [] // ([],[])
//works for all


/*
2. Given lists of lists, process each list and for each number in the list
change their value with the number of times it appears in this list.
Ex.: [[1,1,2,1,2,3], [2]] -> [[3,3,2,3,2,1], [1]]
1 appears 3 times in the first list, so each 1 is changed with 3.
2 appears 2 times in the first list, so each 2 is changed with 2.
3 appears 1 times in the first list, so each 3 is changed with 1.
2 appears 1 times in the second list, so each 2 is changed with 1.
*/
cnt :: Int [Int] -> Int
cnt _ [] = 0
cnt n [x:xs] = length [a\\a<- [x:xs] | a == n] 
//Start = cnt 2 [1,2,3,5,5,5]

cnt1 :: [Int] -> [Int]
cnt1 [] = []
cnt1 [x:xs] = [cnt a [x:xs]\\ a <- [x:xs]] 
//Start = cnt1 [1,1,2,1,2,3]

counter :: [[Int]] -> [[Int]]
counter [] = []
counter list = map cnt1 list

//Start = counter [[1,1,2,1,2,3], [2]] // [[3,3,2,3,2,1],[1]]
//Start = counter [[1,2,1,1,3,4,3],[2,2,4,3,2,2,1]] // [[3,1,3,3,2,1,2],[4,4,1,1,4,4,1]]
//Start = counter [[1..10]] // [[1,1,1,1,1,1,1,1,1,1]]
//Start = counter [] // []
//Start = counter [[1,2,2,3], [], [1,3,4,3,3,1,4]] // [[1,2,2,1],[],[2,3,2,3,3,2,2]]
//works for all


/*
3. Given the list of tuples, where the first element is the list of numbers, the second
element is a bound (Int) and the third one is the switch (Bool). If the switch is true
remove all elements greater than bound from the list, if the switch is false remove all
elements less than bound.
Ex.
*/

filtr :: ([Int],Int,Bool) -> [Int]
filtr (list, bound, switch) 
| switch == True = [a\\a<-list | a <= bound]
= [a\\a<-list | a >= bound]
//Start = filtr ([1..10], 5, False)

dualFilter :: [([Int],Int,Bool)] -> [[Int]]
dualFilter [] = []
dualFilter list = map filtr list

//Start = dualFilter [([1..10], 5, True), ([1..10], 5, False)] // [[1,2,3,4,5],[5,6,7,8,9,10]]
//Start = dualFilter [([3,5..20], 3, True), ([], 4, False), ([1..5], 5, True)] // [[3],[],[1,2,3,4,5]]
//Start = dualFilter [] // []
//Start = dualFilter [([1,3,8,2,12,45,5,1,3,5,81,12], 10, True)] // [[1,3,8,2,5,1,3,5]]
//works for all



