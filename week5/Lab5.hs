module Lab5

where

import Week5
--import Week5_NRC
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
			it "should return a Node list with one or more nodes that contain no constraints, thus a solution for the given sudoku/grid" $ do
				let solutions = solveNs (initNode example1) 
				and (map isSolvedNode solutions) `shouldBe` True
				
		describe "genRandomSudoku" $ do
			it "should generate a solved sudoku starting from a empty sudoku" $ do 
				n <- genRandomSudoku
				isSudoku n && isSolvedNode n `shouldBe` True
				
		describe "genProblem" $ do
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
	Ex.3 sudoku with empty blocks ???
-}

showProblem' :: IO ()
showProblem' = do 
				[r] <- rsolveNs [emptyN]
				showNode $ clearBlocks r
				s  <- genProblem $ clearBlocks r
				showNode s

clearBlocks :: Node -> Node
clearBlocks n = clearBlocks' blocks n
				where blocks = [(r,c) | r <- [1..6], c <- [1..3]]

clearBlocks' :: [(Row,Column)] -> Node -> Node
clearBlocks' [x] n = eraseN n x
clearBlocks' (x:xs) n = clearBlocks' xs (eraseN n x)
				
randomS' = genRandomSudoku >>= showNode . clearBlocks

{-
	Ex. 4/ Ex.5 NRC solver (1 hour)
	INSTRUCTION: first comment import Week5 and uncomment Week5_NRC
-}

{-
	Formal constraint:
	Forall (r,c) in Sudoku s: if both r and c in [2..4] or [6..8] then the value of (r,c) should not exist in other fields that exist within the subGrid that ranges from [2..4] and/or [6..8]. 
	
	Shorter version: the list of values contained in subGrids that range from [2..4] and/or [6..8] should be injective
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