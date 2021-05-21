module pt1010
import StdEnv

/* Define an instance of the built-in class +
for a list of tuples [(a,b)] such that,
the addition of two lists takes place elementwise
(if necessary, the shortest list is extended with zeros to
obtain two lists of equal length).
*/


//Start = [(1,2),(3,4),(7,1)] + [(2,6)] // [(3,8),(3,4),(7,1)]

//Start = [(1.2,3.5)] + [(5.2,0.9),(2.0,6.5)] // [(6.4,4.4),(2,6.5)]


instance + [(a,b)] | +a & +b
    where
        (+) [] [] = []
        (+) [] b = b
        (+) a [] = a
        (+) [a:as] [b:bs] = [((fst a) + (fst b), (snd a)+(snd b))] ++ (as + bs)

