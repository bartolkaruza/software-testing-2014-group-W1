module SS where 

import Data.List
import System.Random

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

-- shows a row of the sudoku ( we do not need to use this)
showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

-- shows the grid  (we don't need this)
showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

-- sudoku is a function from (row, column) pair to a value (range 0 to 9)
type Sudoku = (Row,Column) -> Value

-- creates a grid from a sudoku function
sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

-- creates a sudoku function from a grid
grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

  
-- shows the sudoku (we probably need this)
showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

-- gets a un-nubbed list with all 
bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

-- gets the i, j th subgrid of the sudoku (domain [1..3]^2)
subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]


freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

-- these are the values that haven't been used in the row
freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

-- returns the values that haven't been used in the column
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

-- returns the values that haven't been used in a subgrid
freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

-- returns the values that can be used at a certain (row, column) pair
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c)) 

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

-- if row injective then there are no duplicates in the row
rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

-- if column injective then there are no duplicates in the column
colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

-- if subgrid injective then there are no duplicates in the subgrid
subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))

-- if consistent then there are no duplicates in columns, rows or subgrids
consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]

-- extends a sudoku by filling in a value in a position
extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

-- u
update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

-- a list possible values for a sudoku position
type Constraint = (Row,Column,[Value])

-- a type that defines the sudoku with its contstraints
type Node = (Sudoku,[Constraint])

-- shows the the sudoku defined in the node
showNode :: Node -> IO()
showNode = showSudoku . fst

-- checks if the constraints are empty (then it's solved)
solved  :: Node -> Bool
solved = null . snd

-- extends the node by creating multiple nodes with all the values that are still possible filled in
extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = 
  compare (length zs) (length zs')

  
-- removes the value from the constraints for this row, column and block as its already used
prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest

-- checks if two (row, column pairs) are located in the same block
sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

-- make a Node object from the grid. Calculate constraints using constraints s and return [] if s is inconsistent
initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

-- check if there are any open positions and return these
openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

-- get constraints and sort them from less to more
constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

-- adds nodes as children (grow (update blabla) currentNode)
grow :: (node -> [node]) -> node -> Tree node 
grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal 
                                ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

-- extent the tree by adding all possible values filled as children
succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

-- gets a grid as input and shows the solution
solveAndShow :: Grid -> IO()
solveAndShow gr = solveShowNs (initNode gr)

-- gets a node as input and and shows the input
solveShowNs :: [Node] -> IO()
solveShowNs = sequence_ . fmap showNode . solveNs

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = 
  do n <- getRandomInt maxi
     return [xs !! n]
     where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
        -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs then return []
     else 
       if goal (head xs) then return [head xs]
       else 
        do ys <- rsearch succ goal (succ (head xs))
           if (not . null) ys then return [head ys]
           else 
             if null (tail xs) then return []
             else rsearch succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) = update s ((r,c),0)

-- erases the value from the sudoku
eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

-- removes items until the list of items is empty.. if removing an item makes multiple solutions possible, then it halts
minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) 
   | uniqueSol n' = minimalize n' rcs
   | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

-- gets a list with filled positions
filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = 
  [ (r,c) | r <- positions,  
            c <- positions, s (r,c) /= 0 ]

		
			
-- removes random items one by one until it is a minimum solution
genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s

