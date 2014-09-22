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
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (fs:gs)) = dist (cnf fs) (cnf gs)

dist :: Form -> Form -> Form
dist (Cnj fs) gs = Cnj (map ((flip dist) gs) fs)
dist fs (Cnj gs) = Cnj (map (dist fs) gs)
dist fs gs = Dsj (fs:[gs])
-- we need to test that the 