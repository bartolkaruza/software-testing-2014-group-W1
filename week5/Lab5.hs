module Lab5

where
import Data.List
import Week5
import SS


{- 1. 
 Doing this by quickcheck would require us to be able to generate grids that
   - specify a correct sudoku problem
   - specify a correct sudoku answer
 This means grids that do not have duplicates in rows, columns and subgrids

start - hspec $ do
           describe "SS" $ do
               describe "rsolveNs" 
                   it "returns a random sudoku problem"

                   it "
-}

{- 2. We can test this by generating problems and removing every hint (one at a time) and see if it gives a unique solution
-}

{-testIsMinimal = do [r] <- rsolveNs [emptyN]
                   showNode r
                   s  <- genProblem r
                   all (\x y -> length $ rsolveNs (removeIndex x y s) == 1) [1..9] [1..9]

removeIndex x y s = -}
{- 3. Create a problem, then remove all blocks and put them as children in the tree (like solving). Then 
      do a depth first search to find the one with the most empty blocks. Minimalize this one.
	  
	 Properties when thinking of sudokus with 3 empty blocks
	 - when blocks on one row, the internal rows are interchangable, so it doesn't have a unique solution
	 - when blocks on one column, the internal columns are interchangable
	 - when two blocks on a row or on a column are empty, the blocks are not necessarily interchangable
-}

{- 4. This means adding a constraint which where the blocks' are injective 
      I have implemented this in the file Week5-exercises.hs. 
	  precondition for every sudoku:
      for all i,j,i',j' in [2..4]++[6..8] if grid(i,j)==grid(i',j') then i'==i && j'==j || grid(i,j)==0
	  postconditions:
	  for all i,j,i',j' in [2..4]++[6..8] if grid(i,j)==grid(i',j') then i'==i && j'==j
      for all i,j in [2..4]++[6..8] grid(i,j)<>0                                                            
-}
nrcSudoku :: Grid
nrcSudoku = [[0,0,0,3,0,0,0,0,0],
             [0,0,0,7,0,0,3,0,0],
             [2,0,0,0,0,0,0,0,8],
             [0,0,6,0,0,5,0,0,0],
             [0,9,1,6,0,0,0,0,0],
             [3,0,0,0,7,1,2,0,0],
             [0,0,0,0,0,0,0,3,1],
             [0,8,0,0,4,0,0,0,0],
             [0,0,2,0,0,0,0,0,0]]

{- Solution to NRC Sudoku:
   [[4,7,8,3,9,2,6,1,5],
    [6,1,9,7,5,8,3,2,4],
	[2,3,5,4,1,6,9,7,8],
    [7,2,6,8,3,5,1,4,9],
	[8,9,1,6,2,4,7,5,3],
    [3,5,4,9,7,1,2,8,6],
	[5,6,7,2,8,9,4,3,1],
	[9,8,3,1,4,7,2,8,6],
	[1,4,2,5,6,3,8,9,7]]
-}

{- 5. See implementation in Week5.hs -}

{- 6. The more values can be filled in for the minimal constraint at some point of the solving process the harder it is -}
