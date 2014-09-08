module Lab1Bonus

where

-- Ex 1

foldrLength :: [a] -> Int
foldrLength n = foldr (\x y -> y + 1) 0 n

foldrElem :: (Eq a) => a -> [a] -> Bool
foldrElem x n = foldr (\y z -> (x == y) || z ) False n

foldrOr :: [Bool] -> Bool
foldrOr n = foldr (\x y -> x || y) False n

foldrMap :: (a -> b) -> [a] -> [b]
foldrMap f n = foldr ((:) . f) [] n

foldrFilter :: (a -> Bool) -> [a] -> [a]
foldrFilter f n = foldr (\x y -> if f x then (x:y) else y) [] n

foldrConcat :: [a] -> [a] -> [a]
foldrConcat n m = foldr (\x y -> (x:y)) m n

foldrReverse :: [a] -> [a]
foldrReverse n = foldr (\x y -> y ++ [x]) [] n

-- Ex 2

foldlReverse :: [a] -> [a]
foldlReverse n = foldl (\x y -> [y] ++ x) [] n

-- Ex 3


-- foldr can in some cases return a result from an operation on an infinite list, when only the leftmost argument 
-- is used in the calculation. This is because the foldr operation really works from the left to the right, lazily evaluating
-- each calculation. It creates an infinite datastructure from another infinite data structure in this manner.
-- foldl keeps replacing every next step by a function and needs to turn each step into a function before evaluating a single result.
-- Because of this foldl never reaches evaluation.
-- An example is foldr (:) [] [1..] which forms a new infinite list

-- Ex 4

data Creature = Lady | Tiger 
     deriving (Eq,Show)

sign1' (x,y) = x == Lady || y == Lady
sign2' (x,y) = x == Tiger

solution2 :: [(Creature, Creature)]
solution2 = [ (x,y) | x <- [Lady, Tiger],
					  y <- [Lady, Tiger],
					  sign1'(x,y) == sign2'(x,y) ]

-- Ex 5

data Islander = Knight | Knave deriving (Eq,Show)

john :: (Islander,Islander) -> Bool
john (x,y) = x == y

bill :: (Islander, Islander) -> Bool
bill (x,y) = x /= y

solution3 :: [(Islander,Islander)]
solution3 = [(x,y) | x <- [Knight,Knave], 
                     y <- [Knight,Knave], 
                     john (x,y) == (x == Knight) && bill(x,y) == (y == Knight) ]

-- Ex 6

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

matthew, peter, jack, arnold, carl :: Boy -> Bool
matthew = \ x -> not (x==Matthew) && not (x==Carl)
peter   = \ x -> x==Matthew || x==Jack
jack    = \ x -> not (matthew x) && not (peter x)
arnold  = \ x -> matthew x /= peter x
carl    = \ x -> not (arnold x)

declarations = [matthew,peter,jack,arnold,carl]
table = zip declarations boys 

--solution :: 


