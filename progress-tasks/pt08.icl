module pt08
import StdEnv

/* Create record `City`. It should contain 3 fields: name(String), area(Int)
and population(Int). Write `largeCityCount` function which takes list of Cities
and returns number of cities that have area greater than 300 and population greater than 1 000 000.
*/

:: City = {name:: String, area:: Int, population:: Int}

budapest={name="Budapest", area=525, population=1756000}
kutaisi={name="Kutaisi", area=67, population=147000}
debrecen={name="Debrecen", area=461, population=202000}
berlin={name="Berlin", area=891, population=3645000}
pisa={name="Pisa", area=185, population=90000}

largeCityCount :: [City] -> Int
largeCityCount [] = 0
largeCityCount [x:xs]
| x.area > 300 && x.population > 1000000 = 1 + largeCityCount xs
= largeCityCount xs

//Start = largeCityCount [] // 0
//Start = largeCityCount [budapest,kutaisi,debrecen,berlin,pisa] // 2
//Start = largeCityCount [budapest,berlin] // 2
//Start = largeCityCount [kutaisi] // 0










