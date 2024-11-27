import qualified Data.List
import qualified Data.Array
import qualified Data.Bits


-- PFL 2024/2025 Practical assignment 1

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]


-- 1. cities
{- Extracts a unique list of cities from a given RoadMap.
RoadMap: RoadMap to explore.
Returns a list of City. -}

cities :: RoadMap -> [City]
cities [] = []
cities rm = Data.List.nub (foldr (\(rm_city1, rm_city2, _) acc -> rm_city1 : rm_city2 : acc) [] rm)



-- 2. areAdjacent
{- Checks if two cities are directly connected by an edge in a given RoadMap.
RoadMap: RoadMap to explore.
City, City: Cities to check for adjacency.
Returns true if cities are adjacent and false otherwise. -}

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((rm_city1, rm_city2, _):xs) city1 city2
    | (rm_city1 == city1 && rm_city2 == city2) || (rm_city1 == city2 && rm_city2 == city1) = True
    | otherwise = areAdjacent xs city1 city2



-- 3. distance
{- Outputs the distance between two cities.
RoadMap: The roadmap to explore.
City, City: The two cities to check for a direct connection.
Returns Just dist if the cities are connected, where dist is the distance between them, or Nothing if there is no direct edge. -}

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((rm_city1, rm_city2, dist):xs) city1 city2
    | (rm_city1 == city1 && rm_city2 == city2) || (rm_city1 == city2 && rm_city2 == city1) = Just dist
    | otherwise = distance xs city1 city2



-- 4. adjacent
{- Finds all cities directly connected to a given city in a RoadMap.
RoadMap: The roadmap to explore.
City: The city for which to find adjacent cities.
Returns a list of tuples containing each adjacent city and the corresponding distance. -}

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((rm_city1, rm_city2, dist):xs) city
    | city == rm_city1 = (rm_city2, dist) : adjacent xs city  
    | city == rm_city2 = (rm_city1, dist) : adjacent xs city  
    | otherwise = adjacent xs city



-- 5. pathDistance
{- Calculates the total distance of a given path in a RoadMap.
RoadMap: The roadmap to explore.
Path: The list of cities representing the path.
Returns Just totalDistance if the path is valid, where totalDistance is the sum of distances 
between consecutive cities, or Nothing if any segment of the path is invalid. -}

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0  -- No cities in the path
pathDistance _ [_] = Just 0  -- Only one city
pathDistance rm path =
    foldr (\(c1, c2) acc ->
        let dist = distance rm c1 c2
        in if dist == Nothing || acc == Nothing 
           then Nothing
           else Just (let Just d = dist
                          Just a = acc
                      in d + a) 
    ) (Just 0) (zip path (tail path))



-- 6. rome
{- Identifies the city or cities with the highest in-degree in a RoadMap.
RoadMap: The roadmap to explore.
Returns a list of cities that have the maximum number of incoming edges (in-degrees).
If multiple cities share the highest in-degree, all are included in the result. -}

rome :: RoadMap -> [City]
rome rm = 
    let cityList = cities rm  
        indegrees = [(city, indegreeCount rm city) | city <- cityList]  -- calculate indegrees
        maxIndegree = maximum' (map snd indegrees)  -- maximum indegree
    in [city | (city, indegree) <- indegrees, indegree == maxIndegree] -- filter cities

{- Auxiliary function: inDegreeCount
Counts the number of incoming edges (in-degree) for a given city in a RoadMap.
RoadMap: The roadmap to explore.
City: The city for which to count the incoming edges.
Returns the number of edges that point to the specified city. -}


indegreeCount :: RoadMap -> City -> Int
indegreeCount rm city = 
    foldr (\(_, c2, _) acc -> 
        if c2 == city then acc + 1 
        else acc) 0 rm 


{- Auxiliary function: maximum'
Finds the maximum element in a list.
[a]: The list of elements to evaluate, where elements must be orderable.
Returns the maximum value in the list. Throws an error if the list is empty.
If there is only one element, it returns that element; 
otherwise, it recursively compares elements to find the maximum. -}

maximum' :: Ord a => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x 
maximum' (x:xs) = 
    if x >= maxRest then x 
    else maxRest 
  where
    maxRest = maximum' xs  



-- 7. isStronglyConnected
{- Checks if all cities in a RoadMap are mutually reachable.
RoadMap: The roadmap to explore.
Returns True if every city can be reached from any other city, otherwise returns False.
It does this by performing a depth-first search (DFS) from one city and checking if all cities are reachable. -}

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = allReachable (head citiesList)
  where
    undirectedMap = undirectify roadmap
    citiesList = cities undirectedMap
    allReachable start = reachesEveryCity undirectedMap start citiesList


{- Auxiliary function: undirectify
Converts a directed RoadMap into an undirected one by adding symmetric edges.
RoadMap: The roadmap to convert.
Returns a new RoadMap that includes both the original edges and their reverse counterparts, making all connections bidirectional. -}

undirectify :: RoadMap -> RoadMap
undirectify roadmap = roadmap ++ [(city1, city2, dist) | (city2, city1, dist) <- roadmap]


{- Auxiliary function: adjacencyList
Builds an adjacency list representation of a RoadMap.
RoadMap: The roadmap to convert into an adjacency list.
Returns a list of tuples, where each tuple consists of a city and a list of its adjacent cities along with the corresponding distances. -}

adjacencyList :: RoadMap -> [(City, [(City, Distance)])] 
adjacencyList rm = [(city, adjacent rm city) | city <- cities rm]


{- Auxiliary function: dfs
Performs a depth-first search (DFS) on a RoadMap starting from a specified city.
RoadMap: The roadmap to traverse.
City: The starting city for the DFS.
Returns a list of cities visited during the traversal, in the order they were reached. -}

dfs :: RoadMap -> City -> [City]
dfs rm city = dfs' city []
  where
    dfs' current visited
      | current `elem` visited = visited  -- If already visited, return visited list
      | otherwise = current : concatMap (\(neighbour, _) -> dfs' neighbour (current : visited)) (adjacent rm current)


{- Auxiliary function: reachesEveryCity
Checks if every city in the RoadMap is reachable from a specified starting city.
RoadMap: The roadmap to explore.
City: The starting city from which reachability is checked.
[City]: A list of all cities in the roadmap.
Returns True if all cities can be reached from the starting city, otherwise returns False. -}

reachesEveryCity :: RoadMap -> City -> [City] -> Bool
reachesEveryCity rm start allCities = all (`elem` dfs rm start) allCities



-- 8. shortestPath
{- Finds all shortest paths from a starting city to a destination city in a RoadMap.
RoadMap: The roadmap to explore.
City: The starting city for the search.
City: The destination city for the search.
Returns a list of paths representing the shortest routes from the start city to the destination city. -}

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start dest
    | start == dest = [[start]]  -- If the start and destination are the same, the shortest path is [start]
    | otherwise = Data.List.nub (bfs (adjacencyList (undirectify roadmap)) start dest)


{- Auxiliary function: bfs
Performs a breadth-first search (BFS) to find all shortest paths from a starting city to a destination city.
[(City, [(City, Distance)])]: The adjacency list of the roadmap.
City: The starting city for the BFS.
City: The destination city.
Returns a list of all shortest paths from the start city to the destination city. -}

bfs :: [(City, [(City, Distance)])] -> City -> City -> [Path]
bfs adjList start dest = go [(start, 0)] [(start, [[start]])] []
  where
    go [] _ _ = []  -- If the queue is empty, no more paths to explore
    go ((current, currentDist):queue) pathsMap visited
      | current == dest = lookupPaths dest pathsMap  -- If we reach the destination, return paths
      | otherwise = 
          let newVisited = current : visited
              neighbours = lookupAdj current adjList  -- Pass adjList to lookupAdj
              (newPathsMap, newQueue) = processNeighbours neighbours current currentDist pathsMap queue newVisited
          in go (Data.List.sortOn snd newQueue) newPathsMap newVisited


{- Auxiliary function: lookupAdj
Retrieves the neighbours of a specified city from the adjacency list.
City: The city whose neighbours are being queried.
[(City, [(City, Distance)])]: The adjacency list of the roadmap.
Returns a list of tuples containing neighbouring cities and their corresponding distances. -}

lookupAdj :: City -> [(City, [(City, Distance)])] -> [(City, Distance)]
lookupAdj city adjList = case lookup city adjList of
                           Just ns -> ns
                           Nothing -> []


{- Auxiliary function: lookupPaths
Fetches the list of shortest paths to a specified city from the paths map.
City: The city for which paths are being retrieved.
[(City, [Path])]: A map that associates each city with its list of shortest paths.
Returns a list of paths to the specified city, or an empty list if no paths exist. -}

lookupPaths :: City -> [(City, [Path])] -> [Path]
lookupPaths city pathsMap = case lookup city pathsMap of
                              Just ps -> ps
                              Nothing -> []


{- Auxiliary function: processNeighbours
Processes each neighbour of the current city to update paths and the queue for BFS.
[(City, Distance)]: A list of neighbouring cities and their distances.
City: The current city being processed.
Distance: The distance from the start city to the current city.
[(City, [Path])]: The paths map storing shortest paths to each city.
[(City, Distance)]: The queue of cities to be processed.
[City]: A list of already visited cities.
Returns an updated paths map and queue after processing all neighbours. -}

processNeighbours :: [(City, Distance)] -> City -> Distance -> [(City, [Path])] -> [(City, Distance)] -> [City] -> ([(City, [Path])], [(City, Distance)])
processNeighbours [] _ _ pathsMap queue visited = (pathsMap, queue)
processNeighbours ((neighbour, weight):ns) current currentDist pathsMap queue visited
  | neighbour `elem` visited = processNeighbours ns current currentDist pathsMap queue visited
  | otherwise =
      let newDist = currentDist + weight
          currentPaths = lookupPaths current pathsMap
          existingDist = lookupDistance neighbour queue
          existingPaths = lookupPaths neighbour pathsMap
          
      in if newDist < existingDist
         -- Found a shorter path to `neighbour`
         then processNeighbours ns current currentDist 
                 ((neighbour, map (++ [neighbour]) currentPaths) : filter ((/= neighbour) . fst) pathsMap)
                 ((neighbour, newDist) : filter ((/= neighbour) . fst) queue)
                 visited
         else if newDist == existingDist
              -- Found an equally short path to `neighbour`
              then processNeighbours ns current currentDist 
                     ((neighbour, existingPaths ++ map (++ [neighbour]) currentPaths) : filter ((/= neighbour) . fst) pathsMap)
                     queue
                     visited
              else processNeighbours ns current currentDist pathsMap queue visited


{- Auxiliary function: lookupDistance
Retrieves the distance of a specified city from the queue.
City: The city for which the distance is being queried.
[(City, Distance)]: The queue of cities and their distances.
Returns the distance to the specified city if found, or maxBound if not found, representing an infinitely large distance. -}

lookupDistance :: City -> [(City, Distance)] -> Distance
lookupDistance city queue = case lookup city queue of
                              Just d  -> d
                              Nothing -> maxBound



-- 9. travelSales
{- Applies the Nearest Neighbour Heuristic to find a path for the Traveling Salesman Problem (TSP) in a RoadMap.
RoadMap: The roadmap representing cities and distances.
Returns a path that visits all cities exactly once and returns to the starting city, or an empty list if no valid path exists. -}

travelSales :: RoadMap -> Path
travelSales roadmap 
    | isStronglyConnected roadmap = 
        case cities' of
            [] -> []
            (start:_) -> 
                let path = visit [start] (filter (/= start) cities')
                in if pathDistance roadmap path /= Nothing
                   then path 
                   else []  -- Return empty if the path is not valid
    | otherwise = []
  where
    cities' = cities roadmap

    visit :: Path -> [City] -> Path
    visit path [] = path ++ [head path]  -- Return to starting city
    visit path unvisited = 
        let lastCity = last path
            (nextCity, _) = minimum [(c, distance roadmap lastCity c) | c <- unvisited]
        in visit (path ++ [nextCity]) (filter (/= nextCity) unvisited)

        

--
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
