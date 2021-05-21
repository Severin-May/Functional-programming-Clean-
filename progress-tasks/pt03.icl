module pt03
import StdEnv

//Given a list of integers, write a function that goes through 
//the element, and for every element i,  generates the i-th 
//Fibonnaci number, and only check it's divisible by 3 or not, 
//only keeps the ones divisible by 3.
//Example [0,1,2] -> [] because the 0th fib is 1, the 1st fib 
//is 1 and the 2nd dib is 2,  but only none of them is divisible 
//by 3 so returns an empty list.

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

generateFib :: [Int] -> [Int]
generateFib [] = []
generateFib [x:xs] = [fib x\\x<- [x:xs] | (fib x) rem 3 == 0]

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)

generateFib :: [Int] -> [Int]
generateFib [] = []
generateFib [x:xs]
| (fibo x) rem 3 == 0 = [fibo x: generateFib xs]
= generateFib xs
//generateFib [x:xs] = [fib x\\x<- [x:xs] | (fib x) rem 3 == 0]

//Start = generateFib [0,1,2] // []
//Start = generateFib [3,7,11] //[3,21,144]
//Start = generateFib [4..10] //[21]