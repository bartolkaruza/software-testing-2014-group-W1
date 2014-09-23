module Lab3 where 

import Week3
 
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


main :: String -> IO ()
main s = print (showLst [  ( convert f) | f<- (parse  s)])

-- convert
convert :: Form -> Form
convert f = nnf  (arrowfree f)

cnf :: Form -> Form 
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (x:fs)) = foldl dist x fs

dist :: Form -> Form -> Form
dist (Cnj fs) gs = Cnj (map ((flip dist) gs) fs)
dist fs (Cnj gs) = Cnj (map (dist fs) gs)
dist fs gs = Dsj [fs, gs]
-- we need to test that the 


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

