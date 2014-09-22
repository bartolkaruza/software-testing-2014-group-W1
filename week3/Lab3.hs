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
cnf (Dsj fs) = --distList (map cnf fs)

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


-- cnf: has implemented De Morgan laws
isDeMorgan :: Form -> Bool
isDeMorgan (Prop x) = True
isDeMorgan (Neg (Prop x)) = True
isDeMorgan (Dsj [f1, Cnj [f2, f3]]) = False
isDeMorgan (Dsj [Cnj [f1, f2], f3]) = False
isDeMorgan (Cnj fs) = and (map isDeMorgan fs)
isDeMorgan (Dsj fs) = and (map isDeMorgan fs)


-- failing test : testForms 100 (isDeMorgan . nnf . arrowfree)
-- success test : testForms 100 (isDeMorgan . cnf . nnf . arrowfree)