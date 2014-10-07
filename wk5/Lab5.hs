module Lab5

where

import Data.List
import Week5

import Test.QuickCheck
import Test.Hspec

-- 1. (there are a lot more properties to check. these are some.)

sudokuSpec = hspec $ do
    describe "isFreeInSeq" $ do
        it "should return the non-used values [1..9] in a sequence [1..9]" $ 
            freeInSeq [1,2,4,9] `shouldBe` [3,5,6,7,8]
    describe "isFreeInRow" $ do
        it "should return the non-used values [1..9] in a row of a sudoku" $ 
            freeInRow (grid2sud example1) 3  `shouldBe` [1,2,3,4,5,7]
    describe "isFreeInColumn" $ do
        it "should return the non-used values [1..9] in a column of a sudoku" $ 
            freeInColumn (grid2sud example1) 3  `shouldBe` [1,2,3,4,5,6,7,9]
    describe "isFreeInSubgrid" $ do
        it "should return the non-used values [1..9] in a subgrid of a sudoku" $ 
            freeInSubgrid (grid2sud example1) (1,7)  `shouldBe` [1,2,3,4,5,7,8,9]
    describe "isFreeAtPos" $ do
        it "should return the non-used values [1..9] in a position of a sudoku, adhering to previous freeness properties" $ 
            freeAtPos (grid2sud example1) (1,7)  `shouldBe` [1,4,8,9]

    describe "isInjective" $ do
        it "should return true if a sequence does not contain duplicates" $ 
            injective [1,2,3,4,5] && not (injective [1,1,2,3,4,5]) `shouldBe` True

    describe "isRowInjective" $ do
        it "should return true if a row does not contain duplicates" $ 
            rowInjective (grid2sud example1) 3 `shouldBe` True
    describe "isColInjective" $ do
        it "should return true if a column does not contain duplicates" $ 
            colInjective (grid2sud example1) 3 `shouldBe` True
    describe "subgridInjective" $ do
        it "should return true if a subgrid does not contain duplicates" $ 
            subgridInjective (grid2sud example1) (1,7) `shouldBe` True

    describe "isConsistent" $ do
        it "should return true if a sudoku adheres to the above injectivity properties" $ 
            consistent (grid2sud example1) && not (consistent (grid2sud example4)) `shouldBe` True

{- 2. check on minimality property. 
      remove item from filledPositions until there is no uniqueSol
      first with example
-}

-- prints: [Unique without first filled option, Unique as is]
testMinimality :: IO ()
testMinimality = do [r] <- rsolveNs [emptyN]
                    s  <- genProblem r
                    print $ [remainsUnique (fst s), uniqueSol s]
                    testMinimality

-- removes first filled option, checks if it is still unique
remainsUnique :: Sudoku -> Bool
remainsUnique s = and $ map uniqueSol (initNode (sud2grid (eraseS s (nextPosition s))))

-- selects first filled option
nextPosition :: Sudoku -> (Row,Column)
nextPosition s = head $ filledPositions s

{- 3. Sudoku with three empty blocks (i.e. the default blocks, not any subgrid)

  2.0 hrs

-}

sortGT (a1, b1) (a2, b2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = EQ

-- least filled block, gives [(score, pos)] (WITHOUT ZERO LENGTH)
leastfilled :: Node -> [(Int, (Row, Column))]
leastfilled n = filter (\(x,y) -> x > 0) [ ((length $ freeInSubgrid (fst n) (i,j)) , (i,j)) | i <- [1,4,7], j <- [1,4,7] ]

-- (score, pos) to [(pos)]
extract :: (Int, (Int, Int)) -> [(Row, Column)]
extract (a,(b,c)) = [(i,j) | i <- bl b, j <- bl c]

-- gives 'best' block and picks pos
bestBlock :: Node -> [(Row,Column)]
bestBlock n = extract (head $ sortBy sortGT (leastfilled n))

-- when minimalize' cant continue, find block that has the least filled
-- it fails here, infinite loop
rethink :: Node -> [(Row,Column)] -> [(Row,Column)]
rethink n s | uniqueSol j' = s --(bestBlock n) ++ s
            | otherwise = []
            where j' = eraseN n (head $ bestBlock n)

minimalize' :: Node -> [(Row,Column)] -> Node
minimalize' n [] = n
minimalize' n ((r,c):rcs) 
   | uniqueSol n' = minimalize' n' rcs
   | otherwise    = minimalize' n  (rethink n rcs)
   where n' = eraseN n (r,c)

genEmptyBlockProblem n = showNode (minimalize' n (block1 ++ xs)) 
                         where xs = filledPositions (fst n)

block1 = [ (i,j) | i <- [1..3], j <- [1..3] ]

randomEmpty = genRandomSudoku >>= genEmptyBlockProblem

{- 4. Sudoku generator with extra constraints: subgrid injectivity:
    (left top) (2,2) (2,6) (6,2) (6,6)
    
    1. extend subgrid to find custom 3x3
    2. extend subgridInjective to accept subGridC
    3. extend consistent to include subgridInjectiveC

    4. modify rest of helper functions to solve

    2.0 hrs

-}

subgridC :: Sudoku -> (Row,Column) -> [Value]
subgridC s (r,c) = 
  [ s (r',c') | r' <- [r..r+2], c' <- [c..c+2] ]

subgridInjectiveC :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveC s (r,c) = injective vs where 
   vs = filter (/= 0) (subgridC s (r,c))

freeInSubgridC :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridC s (r,c) = freeInSeq (subgridC s (r,c))

freeAtPosC :: Sudoku -> (Row,Column) -> [Value]
freeAtPosC s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c)) 
   `intersect` (freeInSubgridC s (r,c)) 

consistentC :: Sudoku -> Bool
consistentC s = and $
                [ rowInjective s r |  r <- positions ]
                 ++
                [ colInjective s c |  c <- positions ]
                 ++
                [ subgridInjective s (r,c) | 
                     r <- [1,4,7], c <- [1,4,7]]  
                 ++
                [ subgridInjectiveC s (r,c) | r <- [2,6], c <- [2,6]] 


blocksC :: [[Int]]
blocksC = [[2..4],[6..8]]

blC :: Int -> [Int]
blC x = concat $ filter (elem x) blocksC 

pruneC :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
pruneC _ [] = []
pruneC (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneC (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneC (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneC (r,c,v) rest
  | sameblockC (r,c) (x,y) =                    -- the NRC constraint
        (x,y,zs\\[v]) : pruneC (r,c,v) rest     -- the NRC constraint
  | otherwise = (x,y,zs) : pruneC (r,c,v) rest

sameblockC :: (Row,Column) -> (Row,Column) -> Bool
sameblockC (r,c) (x,y) = blC r == blC x && blC c == blC y 

extendNodeC :: Node -> Constraint -> [Node]
extendNodeC (s,constraintsC) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         pruneC (r,c,v) constraintsC) | v <- vs ]

constraintsC :: Sudoku -> [Constraint] 
constraintsC s = sortBy length3rd 
    [(r,c, freeAtPosC s (r,c)) | 
                       (r,c) <- openPositions s ]

succNodeC :: Node -> [Node]
succNodeC (s,[]) = []
succNodeC (s,p:ps) = extendNodeC (s,ps) p 

initNodeC :: Grid -> [Node] 
initNodeC gr = let s = grid2sud gr in 
               if (not . consistentC) s then [] 
               else [(s, constraintsC s)]

solveAndShowC :: Grid -> IO()
solveAndShowC gr = solveShowNsC (initNodeC gr)

solveNsC :: [Node] -> [Node]
solveNsC = search succNodeC solved 

solveShowNsC :: [Node] -> IO()
solveShowNsC = sequence_ . fmap showNode . solveNsC

nrcExample1 :: Grid
nrcExample1 =  [[0,0,0,3,0,0,0,0,0],
                [0,0,0,7,0,0,3,0,0],
                [2,0,0,0,0,0,0,0,8],
                [0,0,6,0,0,5,0,0,0],
                [0,9,1,6,0,0,0,0,0],
                [3,0,0,0,7,1,2,0,0],
                [0,0,0,0,0,0,0,3,1],
                [0,8,0,0,4,0,0,0,0],
                [0,0,2,0,0,0,0,0,0]]

{- 5. Generator of 'NRC' Sudoku  - 0.5hrs

    1. Convert the random-functions (r-) to work with (-C) functions.

    For sake of completeness, all the IO code is taken.
-}

rsuccNodeC :: Node -> IO [Node]
rsuccNodeC (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNodeC (s,cs\\xs) (head xs))

rsolveNsC :: [Node] -> IO [Node]
rsolveNsC ns = rsearch rsuccNodeC solved (return ns)

genRandomSudokuC :: IO Node
genRandomSudokuC = do [r] <- rsolveNsC [emptyN]
                      return r

randomSC = genRandomSudokuC >>= showNode

uniqueSolC :: Node -> Bool
uniqueSolC node = singleton (solveNsC [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

minimalizeC :: Node -> [(Row,Column)] -> Node
minimalizeC n [] = n
minimalizeC n ((r,c):rcs) 
   | uniqueSolC n' = minimalizeC n' rcs
   | otherwise     = minimalizeC n  rcs
   where n' = eraseN n (r,c)

genProblemC :: Node -> IO Node
genProblemC n = do ys <- randomize xs
                   return (minimalizeC n ys)
    where xs = filledPositions (fst n)

mainC :: IO ()
mainC = do [r] <- rsolveNsC [emptyN]
           showNode r
           s  <- genProblemC r
           showNode s
