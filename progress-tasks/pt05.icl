module pt05
import StdEnv


/*
You are given a tuple of lower and upper bounds and a list of tuples, 
where the first element is course code, the second one is course practice 
score, and the third one is course theory score. Filter the list and return 
the list of course codes, whose average of practice and theory scores 
((p_score+t_score)/2) is between given lower and upper bounds. Bounds are 
included in the range.
*/
/*
rangeFilter :: (Real,Real) [(String,Real,Real)] -> [String]
rangeFilter _ [] = []
rangeFilter (a,b) list = [fst3 x\\x<-list | (((thd3 x)+ (snd3 x))/2.0) >= a && (((thd3 x)+ (snd3 x))/2.0) <= b]
*/
//where
//	avg = (((fst3 x)+ (snd3 x))/2.0)
Start = rangeFilter (10.0,15.0) [("A",12.0,18.0),("B",3.0,15.0),("C",15.0,15.0),("E",14.0,16.0),("F",16.0,12.0)] // ["A","C","E","F"]
//Start = rangeFilter (3.0,7.0) [("A",2.0,8.0),("B",3.0,15.0),("C",15.0,10.0),("E",12.0,6.0),("F",13.0,1.0)] // ["A","F"]
//Start = rangeFilter (5.0,7.0) [] // []
//Start = rangeFilter (15.0,10.0) [("A",12.0,18.0),("B",3.0,15.0),("C",15.0,15.0),("E",14.0,16.0),("F",16.0,12.0)] // []
//Start = rangeFilter (1.0,3.0) [("A",2.0,8.0),("B",3.0,15.0),("C",15.0,10.0),("E",12.0,6.0)] // []


avg :: Real Real -> Real
avg a b = (a+b)/2.0

rangeFilter :: (Real,Real) [(String,Real,Real)] -> [String]
rangeFilter _ [] = []
rangeFilter (l,u) [x:xs] = [fst3 y\\y<-[x:xs] | (avg (snd3 y) (thd3 y)) >= l && (avg (snd3 y) (thd3 y)) <= u]


	