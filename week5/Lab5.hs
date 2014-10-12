module Lab5

where
import Data.List
import Week5
import SS
import Control.Exception (evaluate)
import Test.Hspec
{- You can call all the assignments using: -}
doAll = do
           assignment1
           assignment2 100 -- number of tests to perform
           assignment3 3   -- number of blocks to leave empty
           assignment4
           assignment5 


{-
	We could use QuickCheck to check the 'solving' part of the Sudoku Solver if can construct a Arbitrary function for generating sudokus. It is more difficult to check the generation of sudokus with quickhcheck, because we would have to check the Arbitrary function as well..
-}

assignment1 :: IO()
assignment1 = hspec $ do
    describe "SS" $ do
        describe "solveNs" $ do
            it "should return a Node list with one or more nodes that contain no constraints, thus a solution for the given sudoku/grid" $ do
                let solutions = solveNs (initNode example1) 
                and (map isSolvedNode solutions) `shouldBe` True
				
        describe "genRandomSudoku" $ do
            it "should generate a solved sudoku starting from a empty sudoku" $ do 
                n <- genRandomSudoku
                isSudoku n && isSolvedNode n `shouldBe` True
				
        describe "genProblem" $ do
            it "returns a consistent sudoku" $ do
                [s] <- rsolveNs [emptyN]
                t <- genProblem s
                (consistent (fst t)) `shouldBe` True					  

            it "should generate a unsolved sudoku with an unique solution starting from a solved sudoku" $ do 
                n <- genRandomSudoku
                n' <- genProblem n
                isSudoku n' && not(isSolvedNode n') && uniqueSol n' `shouldBe` True

--main property for a sudoku				
isSudoku :: Node -> Bool
isSudoku n = isCorrectSize g && hasCorrectElems g
		where g = sud2grid $ fst n

--the grid is 9x9
isCorrectSize :: Grid -> Bool
isCorrectSize g = (length g == 9) && and (map (\r -> length r == 9) g)

--all fields contain elements in the range 0..9
hasCorrectElems :: Grid -> Bool
hasCorrectElems g = and (map (\r -> and (map (\c -> elem c [0..9]) r)) g)

--node is solved if it has no constraints and the sudoku is contains unique elements in every row, column and subgrid				
isSolvedNode :: Node -> Bool
isSolvedNode (s,[]) = consistent s
isSolvedNode _ = False

{-
	Ex.2 minimal problems
	1 hour
-}

-- be warned: checkMinimal on 10 cases takes +/- 1 minute
assignment2 :: Int -> IO()
assignment2 count = do 
				n <- genRandomSudoku
				p <- genProblem n
				let r = isMinimal p
				if r == True 
				then 
					if count > 0
					then print $ isMinimal p
					else assignment2 (count-1)
				else print r
				
isMinimal :: Node -> Bool
isMinimal n = uniqueSol n && not (uniqueSol $ removeHint n)

--remove the first hint from the sudoku
removeHint :: Node -> Node
removeHint n = eraseN n (hints!!0)
					 where hints = filledPositions $ fst n
					 
{- 3. Create a problem, then remove all blocks and put them as children in the tree (like solving). Then 
      do a depth first search to find the one with the most empty blocks. Minimalize this one.
	  
	 Properties when thinking of sudokus with 3 empty blocks
	 - when blocks on one row, the internal rows are interchangable, so it doesn't have a unique solution
	 - when blocks on one column, the internal columns are interchangable
	 - when two blocks on a row or on a column are empty, the blocks are not necessarily interchangable
	 
	 In 4 blocks it is dependent on the other blocks. As there are now two blocks empty in at least two columns, it could well be
	 that there are no problems that give a unique solution. For instance when the left 3 subgrids are the right 3 subgrids but flipped
	 
	 In 5 blocks it is impossible, since it is either 3 in a row or the one in the middle and the corners. These would be interchangable.
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
assignment3 :: Int -> IO ()
assignment3 n = do [r] <- rsolveNs [emptyN]
                   showNode r
                   s <- genProblem3B r n
                   showNode s
{- 4. This means adding a constraint which where the blocks' are injective 
      I have implemented this in the file Week5.hs. 
	  precondition for every sudoku:
      for all i,j,i',j' in [2..4]++[6..8] if grid(i,j)==grid(i',j') then i'==i && j'==j || grid(i,j)==0
	  postconditions:
	  for all i,j,i',j' in [2..4]++[6..8] if grid(i,j)==grid(i',j') then i'==i && j'==j
      for all i,j in [2..4]++[6..8] grid(i,j)<>0  

   -- solve sudoku with call solveNrcSudoku nrcSudoku	  
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

			 
assignment4 = do
                print "This is the NRC sudoku:"
                showSudoku' (grid2sud nrcSudoku)
                print "Solving this leads to:"
                [t] <- rsolveNs' [((grid2sud nrcSudoku), constraints (grid2sud nrcSudoku))]	
                showNode' t					
{- Solution to NRC Sudoku:
   [[4,7,8,3,9,2,6,1,5],
    [6,1,9,7,5,8,3,2,4],
	[2,3,5,4,1,6,9,7,8],
    [7,2,6,8,3,5,1,4,9],
	[8,9,1,6,2,4,7,5,3],
    [3,5,4,9,7,1,2,8,6],
	[5,6,7,2,8,9,4,3,1],
	[9,8,3,1,4,7,5,6,2],
	[1,4,2,5,6,3,8,9,7]]
-}

{- 5. See implementation in Week5.hs -}

assignment5 = main'

{- 6. The more values can be filled in for the minimal constraint at some point of the solving process the harder it is 

    -- to create a simple hand solvable solution we need to only remove things that becomes a hidden single or naked single
	-- to create a hard hand solvable solution we need to remove things that require harder techniques to be found



removeHiddenSingle :: Node -> (Row,Column) -> Node
removeHiddenSingle n (r,c) | length (snd (eraseN n (r,c))) == 1 = eraseN n (r,c)
                           | otherwise =  emptyN
							 
-- removes items until the list of items is empty.. if removing an item makes multiple solutions possible, then it halts
minimalizeSimple :: Node -> [(Row,Column)] -> Node
minimalizeSimple n [] = n
minimalizeSimple n ((r,c):rcs) 
   | uniqueSol n' = minimalizeSimple n' rcs
   | otherwise    = minimalizeSimple n  rcs
  where n' = removeHiddenSingle n (r,c)
  	
-- removes random items one by one until it is a minimum solution
genProblemSimple :: Node -> IO Node
genProblemSimple n = do ys <- randomize xs
                        return (minimalizeSimple n ys)
                     where xs = filledPositions (fst n)

simpleSudoku :: IO ()
simpleSudoku = do [r] <- rsolveNs [emptyN]
                  showNode r
                  s  <- genProblemSimple r
                  showNode s
-}