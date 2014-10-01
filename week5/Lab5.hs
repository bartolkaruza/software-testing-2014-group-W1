module Lab5

where
import Data.List
import Week5



{- 1. 
 Doing this by quickcheck would require us to be able to generate grids that
   - specify a correct sudoku problem
   - specify a correct sudoku answer

-}



{- 2. We can test this by generating problems and removing every hint (one at a time) and see if it gives a unique solution
-}


{- 3. Create a problem, then remove all blocks and put them as children in the tree (like solving). Then 
      do a depth first search to find the one with the most empty blocks. Minimalize this one.
	  
	 Properties when thinking of sudokus with 3 empty blocks
	 - when blocks on one row, the internal rows are interchangable, so it doesn't have a unique solution
	 - when blocks on one column, the internal columns are interchangable
	 - when two blocks on a row or on a column are empty, the blocks are not necessarily interchangable
-}

{- 4. This means adding a constraint which where the blocks' are injective -}

{- 6. The more values can be filled in for the minimal constraint at some point of the solving process the harder it is -}

blocks' :: [[Int]]
blocks' = [[2..4],[6..8]]

-- gets a un-nubbed list with all 
bl' :: Int -> [Int]
bl' x = concat $ filter (elem x) blocks' 

-- gets the i, j th subgrid of the sudoku 
subGrid' :: Sudoku -> (Row,Column) -> [Value]
subGrid' s (r,c) = 
  [ s (r',c') | r' <- bl' r, c' <- bl' c ]

-- returns the values that haven't been used in a subgrid
freeInSubgrid' :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid' s (r,c) = freeInSeq (subGrid' s (r,c))

-- if subgrid injective then there are no duplicates in the subgrid
subgridInjective' :: Sudoku -> (Row,Column) -> Bool
subgridInjective' s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid' s (r,c))


-- if consistent then there are no duplicates in columns, rows or subgrids
consistent' :: Sudoku -> Bool
consistent' s = consistent s && and $ [ subgridInjective' s (r,c) | 
                    r <- [2,6], c <- [2,6]]   

-- make a Node object from the grid. Calculate constraints using constraints s and return [] if s is inconsistent
initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in 
              if (not . consistent') s then [] 
              else [(s, constraints s)]
			  
-- gets a grid as input and shows the solution
solveAndShow' :: Grid -> IO()
solveAndShow' gr = solveShowNs (initNode' gr)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s
		  
	