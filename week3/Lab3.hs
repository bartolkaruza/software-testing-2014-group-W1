module Lab3 where 

import Week3
import Data.List
 -- time spent (1 minute)
contradiction :: Form -> Bool
contradiction f =  not (satisfiable f)

-- time spent (1 minute)
tautology :: Form -> Bool
tautology f = contradiction (Neg f)

-- time spent (5 min)
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

-- time spent (5 min)
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)


main :: Form -> Form
main f = cnf (convert f)

-- convert
convert :: Form -> Form
convert f = nnf  (arrowfree f)

cnf :: Form -> Form 
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (f:fs)) = foldl dist f fs

dist :: Form -> Form -> Form
dist (Cnj fs) gs = Cnj (map ((flip dist) gs) fs)
dist fs (Cnj gs) = Cnj (map (dist fs) gs)
dist fs gs = Dsj [fs, gs]


testCnf :: Int -> (Form->Bool) -> IO()
testCnf n f = test n f (map main (getRandomFs n))

-- we need to test that there are no conjunctions in a disjunction
cnfCnj :: Form -> Bool
cnfCnj (Prop x) = True
cnfCnj (Impl f g) = False
cnfCnj (Equiv f g) = False
cnfCnj (Neg f) = cnfCnj f
cnfCnj (Cnj (f:[])) = cnfCnj f
cnfCnj (Cnj (f:fs)) = (cnfCnj f) && cnfCnj (Cnj fs)
cnfCnj (Dsj ((Cnj f):fs)) = False
cnfCnj (Dsj (f:[])) = cnfCnj f
cnfCnj (Dsj (f:fs)) = (cnfCnj f) && cnfCnj (Dsj fs)


-- validity checker for CNF formula's (not needed for exercises)
cnfValid :: Form -> Bool
cnfValid (Prop x) = True
cnfValid (Neg (Prop x)) = True
cnfValid (Cnj (f:fs)) = (cnfValid f) && cnfValid (Cnj fs)
cnfValid (Dsj ((Prop x):fs)) = (and $ (map (==show (Neg (Prop x))) (map show fs))) && cnfValid (Dsj (delete (Neg (Prop x)) (delete (Prop x) fs)))
cnfValid (Dsj (f:fs)) = (cnfValid f) && cnfValid (Dsj fs)
cnfValid _ = False



-- clauses
type Clause = [Int]
type Clauses = [Clause]

--cnf2cls :: Form -> Clauses
--cnf2cls (Prop x) = [[x]]
--cnf2cls (Neg (Prop x)) = cnf2cls (Prop (-x))
--cnf2cls (Cnj fs) = concatMap cnf2cls fs
--cnf2cls (Dsj (f:fs)) = concat [cnf2clse f]:(cnf2cls (Dsj fs))

--cnf2clse :: Form -> Clause
--cnf2clse (Prop x) = [x]
--cnf2clse (Neg (Prop x)) = [-x]
--cnf2clse (Dsj (f:fs)) = concat (cnf2clse f) : (cnf2clse (Dsj fs))

