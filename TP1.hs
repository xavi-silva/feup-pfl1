import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--1
cities :: RoadMap -> [City]
cities [] = []
cities rm = Data.List.nub $ foldr (\(rm_city1, rm_city2, _) acc -> rm_city1 : rm_city2 : acc) [] rm

--2
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((rm_city1, rm_city2, _):xs) city1 city2
    | (rm_city1 == city1 && rm_city2 == city2) || (rm_city1 == city2 && rm_city2 == city1) = True
    | otherwise = areAdjacent xs city1 city2

--3
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((rm_city1, rm_city2, dist):xs) city1 city2
    | (rm_city1 == city1 && rm_city2 == city2) || (rm_city1 == city2 && rm_city2 == city1) = Just dist
    | otherwise = distance xs city1 city2

--4
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

--5
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

--6
rome :: RoadMap -> [City]
rome = undefined

--7
{-
dfs :: RoadMap -> City -> [City] -> [City]
dfs _ _ [] = []
dfs roads current visited
    | current `elem` visited = visited
    | otherwise = foldl (\v (city1, city2, _) -> 
                            if city1 == current && city2 `notElem` v
                            then dfs roads city2 v
                            else if city2 == current && city1 `notElem` v
                            then dfs roads city1 v
                            else v
                        ) (current:visited) roads

dfs :: [City] -> [Int] -> Int -> [Int]
dfs rm visited node = helper rm visited (rm !! node) node
    where helper _ visited [] _ = visited
          helper rm visited (x:xs) currNode
            | elem x visited = helper rm visited xs currNode
            | otherwise = dfs rm (currNode:visited) x

            
-- Main function: Check if the rm is strongly connected
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm =
    let rm_cities = cities rm
        uniqueCities = foldr (\city acc -> if city `elem` acc then acc else city:acc) [] rm_cities
    in case uniqueCities of
        [] -> True  -- An empty roadmap is trivially connected
        (start:_) -> let visited = dfs (cities rm) start []
                     in length visited == length uniqueCities -}

--8
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

--9
travelSales :: RoadMap -> Path
travelSales = undefined

--
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some rms to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected rm
gTest3 = [("0","1",4),("2","3",2)]