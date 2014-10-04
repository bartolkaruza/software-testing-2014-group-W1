module Lab3 where 

import Data.List
import System.Random
import Week3

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\v -> eval v f) (allVals f)

-- logical entailment
entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = (entails a b) && (entails b a)

{-

-- foldl does not work

cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj []) = Dsj []
cnf (Dsj (f:fs)) = foldl dist f fs
--cnf (Dsj [f1, f2]) = dist (cnf f1) (cnf f2)
--cnf (Dsj [f1, f2]) = dist (cnf f1) (cnf f2)
cnf x = error (show x)

dist :: Form -> Form -> Form
dist (Cnj [f1, f2]) fs = Cnj [(dist f1 fs), (dist f2 fs)] -- (p & q) | r -> (p | r) & (q | r)
dist fs (Cnj [f1, f2]) = Cnj [(dist fs f1), (dist fs f2)] -- p | (q & r) -> (p | q) & (p | r)
dist a b = Dsj [a,b]

-}


-- from: repo axel
-- start
--preconditions: input is in NNF
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj fs) = Cnj(map cnf fs)
cnf (Dsj []) = Dsj[]                --distList cannot handle empty list
cnf (Dsj fs) = distList (map cnf fs)

--apply distribution laws on list of forms
--precondition: every form is in cnf
distList :: [Form] -> Form
distList [] = error "should not come here"
distList [f] = f
distList (f:fs) = dist f (distList fs)

--precondition: f1 and f2 are in cnf
dist :: Form -> Form -> Form
dist (Cnj f1) f2 = Cnj(map (\f -> (dist f f2)) f1)
dist f1 (Cnj f2) = Cnj(map (\f -> (dist f1 f)) f2)
dist f1 f2 = Dsj[f1, f2]
-- end


-- ex 3

-- cnf: has implemented distribution law
isCnf :: Form -> Bool
isCnf (Prop x) = True
isCnf (Neg (Prop x)) = True
isCnf (Dsj [f1, Cnj [f2, f3]]) = False
isCnf (Dsj [Cnj [f1, f2], f3]) = False
isCnf (Cnj fs) = and (map isCnf fs)
isCnf (Dsj fs) = and (map isCnf fs)


-- failing test : testForms 100 (isCnf . nnf . arrowfree)
-- success test : testForms 100 (isCnf . cnf . nnf . arrowfree)

-- ex 4

type Clause = [Int]
type Clauses = [Clause]

-- does not work either...

{-

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[-x]]
cnf2cls (Cnj fs) = map cnf2cls' fs
cnf2cls (Dsj fs) = [foldl (++) [] (map cnf2cls' fs)]

cnf2cls' :: Form -> Clause
cnf2cls' (Prop x) = [x]
cnf2cls' (Neg (Prop x)) = [-x]
--cnf2cls' (Cnj fs) = foldl (++) [] (map cnf2cls' fs)
--cnf2cls' (Dsj fs) = foldl (++) [] (map cnf2cls' fs)


cnf2cls :: Form -> Clauses -- returns [[Clause],[Clause],[Clause],...]
cnf2cls (Cnj fs) = map cnf2cls' fs -- Cnj -> 
cnf2cls x = [cnf2cls' x]

cnf2cls' :: Form -> Clause -- returns [Int, Int, Int,...]
cnf2cls' (Dsj fs) = map cnf2cls' fs
cnf2cls' (Prop x) = [x]
cnf2cls' (Neg (Prop x)) = [-x]

-}

cnf2cls :: Form -> Clauses
cnf2cls (Cnj fs) = (map clsDsj fs)
cnf2cls f = [[clsProp f]]

clsDsj :: Form -> Clause
clsDsj (Dsj fs) = (map clsProp fs)
clsDsj f = [clsProp f]

clsProp :: Form -> Int
clsProp (Prop x) = x
clsProp (Neg (Prop x)) = -1*x

{-

testCls :: Clauses -> Bool
testcls [x] = True
testcls ((x:y):) = True

-}