module Week5 where 

import Data.List
import System.Random
import SS

blocksNRC :: [[Int]]
blocksNRC = [[2..4],[6..8]]

-- shows a row of the sudoku ( we do not need to use this)
showRow' :: [Value] -> IO()
showRow' [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' ' ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) 
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showRowSplit [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putChar '|'         
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) 
     putChar '|'         ; putChar ' '
     putStr (showVal a4)  
     putChar '|'        
     putStr (showVal a5)
     putChar '|'         
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) 
     putChar '|'         
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'	 
	
	
-- shows the grid  (we don't need this)
showGrid' :: Grid -> IO()
showGrid' [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow' as
    putStrLn ("|   +---+--+ +--+----+  |")
    showRowSplit bs; showRowSplit cs
    putStrLn ("+-------+-------+-------+")
    showRowSplit ds; 
    putStrLn ("|   +---+--+ +--+----+  |")
    showRow' es
    putStrLn ("|   +---+--+ +--+----+  |")
    showRowSplit fs
    putStrLn ("+-------+-------+-------+")
    showRowSplit gs; showRowSplit hs
    putStrLn ("|   +---+--+ +--+----+  |")
    showRow' is
    putStrLn ("+-------+-------+-------+")
  
-- shows the sudoku (we probably need this)
showSudoku' :: Sudoku -> IO()
showSudoku' = showGrid' . sud2grid

-- gets a un-nubbed list with all 
blNRC :: Int -> [Int]
blNRC x = concat $ filter (elem x) blocksNRC 

-- gets the i, j th subgrid of the sudoku (domain [1..3]^2)
subGridNRC :: Sudoku -> (Row,Column) -> [Value]
subGridNRC s (r,c) = 
  [ s (r',c') | r' <- blNRC r, c' <- blNRC c ]
 
-- returns the values that haven't been used in a subgrid
freeInSubgridNRC :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNRC s (r,c) = freeInSeq (subGridNRC s (r,c))

-- returns the values that can be used at a certain (row, column) pair
freeAtPos' :: Sudoku -> (Row,Column) -> [Value]
freeAtPos' s (r,c) = freeAtPos s (r,c)
                      `intersect` (freeInSubgridNRC s (r,c))    

-- if subgrid injective then there are no duplicates in the subgrid
subgridInjectiveNRC :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNRC s (r,c) = injective vs where 
   vs = filter (/= 0) (subGridNRC s (r,c))

-- if consistent then there are no duplicates in columns, rows or subgrids
consistent' :: Sudoku -> Bool
consistent' s = consistent s && and [ subgridInjectiveNRC s (r,c) | 
                    r <- [2,6], c <- [2,6]]


-- shows the the sudoku defined in the node
showNode' :: Node -> IO()
showNode' = showSudoku' . fst


-- extends the node by creating multiple nodes with all the values that are still possible filled in
extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints') (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune' (r,c,v) constraints') | v <- vs ]
  
-- removes the value from the constraints for this row, column and block as its already used
prune' :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblockNRC (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune' (r,c,v) rest
  | otherwise = (x,y,zs) : prune' (r,c,v) rest

-- checks if two (row, column pairs) are located in the same block
sameblockNRC :: (Row,Column) -> (Row,Column) -> Bool
sameblockNRC (r,c) (x,y) = blNRC r == blNRC x && blNRC c == blNRC y 

-- make a Node object from the grid. Calculate constraints using constraints s and return [] if s is inconsistent
initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in 
              if (not . consistent') s then [] 
              else [(s, constraints' s)]

-- get constraints and sort them from less to more
constraints' :: Sudoku -> [Constraint] 
constraints' s = sortBy length3rd 
    [(r,c, freeAtPos' s (r,c)) | 
                       (r,c) <- openPositions s ]

solveNs' :: [Node] -> [Node]
solveNs' = search succNode' solved 

-- extent the tree by adding all possible values filled as children
succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p 

-- gets a grid as input and shows the solution
solveAndShow' :: Grid -> IO()
solveAndShow' gr = solveShowNs' (initNode' gr)

-- gets a node as input and and shows the input
solveShowNs' :: [Node] -> IO()
solveShowNs' = sequence_ . fmap showNode' . solveNs'

emptyN' :: Node
emptyN' = (\ _ -> 0,constraints' (\ _ -> 0))

rsuccNode' :: Node -> IO [Node]
rsuccNode' (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNode' (s,cs\\xs) (head xs))

rsolveNs' :: [Node] -> IO [Node]
rsolveNs' ns = rsearch rsuccNode' solved (return ns)

genRandomSudoku' :: IO Node
genRandomSudoku' = do [r] <- rsolveNs' [emptyN']
                      return r

randomS' = genRandomSudoku' >>= showNode'

uniqueSol' :: Node -> Bool
uniqueSol' node = singleton (solveNs' [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

-- erases the value from the sudoku
eraseN' :: Node -> (Row,Column) -> Node
eraseN' n (r,c) = (s, constraints' s) 
  where s = eraseS (fst n) (r,c) 

-- removes items until the list of items is empty.. if removing an item makes multiple solutions possible, then it halts
minimalize' :: Node -> [(Row,Column)] -> Node
minimalize' n [] = n
minimalize' n ((r,c):rcs) 
   | uniqueSol' n' = minimalize' n' rcs
   | otherwise    = minimalize' n  rcs
  where n' = eraseN' n (r,c)

-- removes random items one by one until it is a minimum solution
genProblem' :: Node -> IO Node
genProblem' n = do ys <- randomize xs
                   return (minimalize' n ys)
     where xs = filledPositions (fst n)

main' :: IO ()
main' = do [r] <- rsolveNs' [emptyN']
           showNode' r
           s  <- genProblem' r
           showNode' s
