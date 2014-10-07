module Lab5

where

--import Week5
import Week5_NRC
import Data.List
import Test.QuickCheck
import Test.Hspec

{-
	Ex.1 Hspec props
	1.5 hour
	
	We could use QuickCheck to check the 'solving' part of the Sudoku Solver if can construct a Arbitrary function for generating sudokus. It is more difficult to check the generation of sudokus with quickhcheck, because we would have to check the Arbitrary function as well..
-}

main :: IO()
main = hspec $ do
	describe "SS" $ do
		describe "solveNs" $ do
			it "should return a Node list with one or more nodes that have sudokus without constraints or empty boxes, thus a solution for the given sudoku/grid" $ do
				let solutions = solveNs (initNode example1) 
				and (map (\n -> snd n == [] && (length $ openPositions $ fst n) == 0) solutions) `shouldBe` True
				
		describe "genRandomSudoku" $ do
			it "returns a node containing a sudoku" $ do 
				n <- genRandomSudoku
				isSudoku n `shouldBe` True
			
			it "returns a node that contains no empty boxes" $ do
			    [s] <- rsolveNs [emptyN]     
			    length (openPositions (fst s)) `shouldBe` 0
		
			it "returns a node containing a solved sudoku, without constraints" $ do 
				n <- genRandomSudoku
				(snd n) == [] `shouldBe` True
				 
			it "returns a node that contains a consistent sudoku in that it contains no duplicates in rows, columns, subgrids" $ do
				n <- genRandomSudoku
				consistent (fst n) `shouldBe` True

				
		describe "genProblem" $ do
			it "returns a sudoku" $ do 
				n <- genRandomSudoku
				n' <- genProblem n
				isSudoku n' `shouldBe` True
			
			it "returns a node that contains more than 1 empty box" $ do
				n <- genRandomSudoku
				n' <- genProblem n
				length (openPositions (fst n')) > 0 `shouldBe` True
			   
			it "returns an unsolved sudoku, with constraints" $ do
				n <- genRandomSudoku
				n' <- genProblem n
				(snd n') /= [] `shouldBe` True 
			   
			it "returns a problem with a unique solution" $ do
				n <- genRandomSudoku
				n' <- genProblem n
				(uniqueSol n') `shouldBe` True    

				
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

{-
	Ex.2 minimal problems
	1 hour
-}

-- be warned: checkMinimal on 10 cases takes +/- 1 minute
checkMinimal :: Int -> IO()
checkMinimal count = do 
				n <- genRandomSudoku
				p <- genProblem n
				let r = isMinimal p
				if r == True 
				then 
					if count > 0
					then print $ isMinimal p
					else checkMinimal (count-1)
				else print r
				
isMinimal :: Node -> Bool
isMinimal n = uniqueSol n && not (uniqueSol $ removeHint n)

--remove the first hint from the sudoku
removeHint :: Node -> Node
removeHint n = eraseN n (hints!!0)
					 where hints = filledPositions $ fst n
					 
					 
{- 
	Ex.3 sudoku with empty blocks
	
	 - when empty blocks on one row, the internal rows are interchangable, so it doesn't have a unique solution
	- when empty blocks on one column, the internal columns are interchangable,, so it doesn't have a unique solution
	- when two empty blocks on a row or on a column are empty, the blocks are not necessarily interchangable, so it could have a unique solution
-}

--first minimize certain blocks, then generate a problem on that
showProblem' :: IO ()
showProblem' = do 
				[r] <- rsolveNs [emptyN]
				showNode $ r
				s <- genProblem $ minimalizeBlocks $ r
				showNode $ s

--minimize blocks in sudoku diagonally (for 3 empty blocks)			
minimalizeBlocks :: Node -> Node
minimalizeBlocks n = minimalize n xs
   where xs = [(r,c) | r <- [1..3], c <- [1..3]] ++
				--[(r,c) | r <- [7..9], c <- [1..3]] ++ 
				--use for four blocks, does not work always..
			      [(r,c) | r <- [4..6], c <- [4..6]] ++
			         [(r,c) | r <- [7..9], c <- [7..9]]

{-
	Ex. 4/ Ex.5 NRC solver (1 hour)
	INSTRUCTION: first comment import Week5 and uncomment Week5_NRC
	
	Extra functions:
	
	nrcBlocks :: [[Int]]
	nrcBl :: Int -> [Int]
	nrcSubgridInjective :: Sudoku -> (Row,Column) -> Bool
	nrcSameBlock (r,c) (x,y) = nrcBl r == nrcBl x && nrcBl c == nrcBl y
	
	Extensions done on: 
	consistent
	prune
-}

{-
	Formal constraint:
	Forall (r,c) in Sudoku s: if both r and c in [2..4] or [6..8] then the value of (r,c) should not exist in other fields that exist within the subGrid that ranges from [2..4] and/or [6..8]. 
	
	Shorter version: the list of values contained in subGrids that range from [2..4] and/or [6..8] should be unique
-}


nrcExample1 :: Grid
nrcExample1 = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]
			
{- solveAndShow nrcExample1 gives:
	+---------+---------+---------+
	| 4   7 8 | 3  9  2 | 6 1   5 |
	|   +-----|--+   +--|-----+   |
	| 6 | 1 9 | 7| 5 |8 | 3 2 | 4 |
	| 2 | 3 5 | 4| 1 |6 | 9 7 | 8 |
	+---------+---------+---------+
	| 7 | 2 6 | 8| 3 |5 | 1 4 | 9 |
	|   +-----|--+   +--|-----+   |
	| 8   9 1 | 6  2  4 | 7 5   3 |
	|   +-----|--+   +--|-----+   |
	| 3 | 5 4 | 9| 7 |1 | 2 8 | 6 |
	+---------+---------+---------+
	| 5 | 6 7 | 2| 8 |9 | 4 3 | 1 |
	| 9 | 8 3 | 1| 4 |7 | 5 6 | 2 |
	|   +-----|--+   +--|-----+   |
	| 1   4 2 | 5  6  3 | 8 9   7 |
	+---------+---------+---------+
-}

{-
	Ex.6
-}

{-
	Constraint relaxation is probably more easy applicable than the other approaches (involving computational models) described by Pelanek.
	
	The increase in number of solutions after relaxing constraints (by removing hints) should indicate the relative difficulty of the sudoku compared to other sudoku's.
	
	--> randomly removing hint --> count number of constraints added in Node --> count the increase in number of solutions the solver comes up with
-}