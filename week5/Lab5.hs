module Lab5

where
import Data.List
import Week5
import SS
import Control.Exception (evaluate)
import Test.Hspec


{- 1. 
 Doing this by quickcheck would require us to be able to generate grids that
   - specify a correct sudoku problem
   - specify a correct sudoku answer
 This means grids that do not have duplicates in rows, columns and subgrids, which we can't accomplish using QuickCheck
-}
start = hspec $ do
           describe "SS" $ do
               describe "rsolveNs" $ do 
                   it "returns a node that contains no empty boxes" $ do
                      [s] <- rsolveNs [emptyN]	 
                      length (openPositions (fst s)) `shouldBe` 0

                   it "returns a node that is consistent in that it contains no duplicates in rows, columns, subgrids" $ do
                      [s] <- rsolveNs [emptyN]	 
                      (consistent (fst s)) `shouldBe` True  

               describe "genProblem" $ do
                   it "returns a consistent sudoku" $ do
                      [s] <- rsolveNs [emptyN]
                      t <- genProblem s
                      (consistent (fst t)) `shouldBe` True					  

                   it "returns a problem with a unique solution" $ do
                      [s] <- rsolveNs [emptyN]
                      t <- genProblem s
                      (uniqueSol t) `shouldBe` True	


{- 2. We can test this by generating problems and removing every hint (one at a time) and see if it gives a unique solution
-}

testIsMinimal = do
               [r] <- rsolveNs [emptyN]
               s   <- genProblem r
               return $ not (and $ [uniqueSol ((eraseS (fst s) (x,y)), constraints (eraseS (fst s) (x,y))) | x <- [1..9], y <- [1..9]])

{- 3. Create a problem, then remove all blocks and put them as children in the tree (like solving). Then 
      do a depth first search to find the one with the most empty blocks. Minimalize this one.
	  
	 Properties when thinking of sudokus with 3 empty blocks
	 - when blocks on one row, the internal rows are interchangable, so it doesn't have a unique solution
	 - when blocks on one column, the internal columns are interchangable
	 - when two blocks on a row or on a column are empty, the blocks are not necessarily interchangable
-}

-- gets a list with filled positions
filledBlocks :: Sudoku -> [(Row,Column)]
filledBlocks s = 
  [ (r,c) | r <- [1..9],  
            c <- [1..9], maximum (subGrid s (r,c)) /= 0 ]	
			
minimalizeB :: Node -> [(Row,Column)] -> Node
minimalizeB n ((r,c):rcs) | length (freeInSubgrid (fst (minimalize n sg)) $ head sg) == 9 = (minimalize n sg)
                          | otherwise = minimalizeB n rcs
                           where sg = [(r',c') | r'<- bl r, c' <- bl c]

						   
-- removes random items one by one until it is a minimum solution
genProblem3B :: Node -> Int -> IO Node
genProblem3B n x = if x == 0 then return n
                   else
                       do ys <- randomize xs
                          (genProblem3B (minimalizeB n ys) (x-1))
                       where xs = filledBlocks (fst n)
test :: IO ()
test = do [r] <- rsolveNs [emptyN]
          showNode r
          s <- genProblem3B r 3
          showNode s
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
