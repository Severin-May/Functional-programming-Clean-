module pt04
import StdEnv
/*
You are given two lists. The first list contains a list 
of arguments [x, y], and the second list contains a list of 
coefficients [a, b, c] for equation a*x2-b*y2-c*y*x. Your task 
is to calculate the result of each equation.
Example: [[1,2],[1,2]] [[1,2,3], [0,1,-1]] -> [-13,-2] , because 
the 1st equation is 1*12-2*22-3*2*1 and 2nd equation is 0*12-1*22-(-1)*2*1.

Note: You are guaranteed that lists have the same size.
*/
func :: [Int] [Int] -> Int
func args coefs = ((hd coefs)* hd(args)^2) - ((coefs!!1)*last(args)^2) - (last(coefs)*hd(args)*last(args))
//Start = func [1,2][1,2,3] //-11
//Start = func [1,1][1,1,1]
/*
solver :: [[Int]]  [[Int]] -> [Int]
solver [] [] = []
solver arg coef = [func x y\\x<- arg & y<- coef]
*/
//Start = solver []  [] // []
//Start = solver [[1,2]] [[1,2,3]] // [-13]
//Start = solver [[1,1],[2,0],[5,1]]  [[1,1,1],[2,3,5],[10,0,0]] // [-1,8,250]
//Start = solver [[1,2],[1,2]]  [[1,2,3], [0,1,-1]] // [-13,-2]
//Start = solver [[1,-1],[2,3],[5,1],[-5,0],[0,0]]  [[1,3,1],[2,7,1],[10,0,0],[0,0,0],[2,2,2]] // [-1,-61,250,0,0





