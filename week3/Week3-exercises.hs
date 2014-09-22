module Main where
	
	import Week3
	
	type Clause = [Int]
	type Clauses = [Clause]
	
	main :: IO ()
	main = return()

------------------------------------
--Ex.1 (1.5 hours)
	
	form4 = (Impl p q)
	form5 = (Impl p (Dsj[p, q]))
	form6 = (Dsj[Neg p, q])
	form7 = (Impl (Neg q) (Neg p))	

--no valuation should be true
	contradiction :: Form -> Bool
	contradiction f = not (satisfiable f)
	--contradiction form1 				False
	--contradiction (Impl p (Neg p) 	True
	
--all valuations should be true
	tautology :: Form -> Bool
	tautology f = all (\v -> eval v f) (allVals f)
	--tautology form1 	True
	--tautology form2 	False
		
	entails :: Form -> Form -> Bool
	entails f g = tautology (Impl f g)
	--entails form4 form5 		True
	--entails form5 form4		False
	
	equiv :: Form -> Form -> Bool
	equiv f g = (entails f g) && (entails g f)
	--entails form4 form5		False
	--entails form4 form6		True
	--entails form4 form7 		True
	
---------------------------------------
--Ex.2 CNF  (2 hours)

	{-
		preconditions: input is in NNF
		postconditions: output is in (nested) CNF
	-}
	cnf :: Form -> Form
	cnf (Prop x) = Prop x
	cnf (Neg (Prop x)) = Neg (Prop x)
	cnf (Cnj fs) = Cnj(map cnf fs)
	cnf (Dsj []) = Dsj[]				--distList cannot handle empty list
	cnf (Dsj fs) = distList (map cnf fs)

	--apply distribution laws on list of forms
	--precondition: input is in cnf
	distList :: [Form] -> Form
	distList [] = error "should not come here"
	distList [f] = f
	distList (f:fs) = dist f (distList fs)
	
	--precondition: two inputs are in cnf
	dist :: Form -> Form -> Form
	dist (Cnj f1) f2 = Cnj(map (\f -> (dist f f2)) f1)
	dist f1 (Cnj f2) = Cnj(map (\f -> (dist f1 f)) f2)
	dist f1 f2 = Dsj[f1, f2]
	
	--main conversion function
	convertCNF :: Form -> Form
	convertCNF f = cnf (nnf (arrowfree (f)))
	
	test1 = (Neg (Neg (Neg (p))))
	test2 = (Neg (Dsj([p, (Neg q)])))
	test3 = (Neg (Cnj([Neg(p), Neg(q)])))
	test4 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
	--distribution laws
	test5 = Dsj[(Cnj[p,q]), r]
	test6 = Dsj[p, (Cnj[q,r])]
	test7 = Impl (Cnj[(Neg(p)), (Neg q)]) p
	
	
-------------------------------------------
--Ex.3 CNF test
	
	testAF :: Form -> Bool
	testAF (Prop x) = True
	testAF (Neg f) = testAF f
	testAF (Cnj fs) = and (map testAF fs)
	testAF (Dsj fs) = and (map testAF fs)
	testAF (Impl f1 f2) = False
	testAF (Equiv f1 f2) = False
	
	testNNF :: Form -> Bool
	testNNF (Prop x) = True
	testNNF (Neg (Prop x)) = True		--only negation that is allowed
	testNNF (Neg f) = False				--for every cnj/dsj within negation
	testNNF (Cnj fs) = and (map testNNF fs)
	testNNF (Dsj fs) = and (map testNNF fs)
		
	testCNF :: Form -> Bool
	testCNF (Prop x) = True
	testCNF (Neg (Prop x)) = True
	--testCNF [(Dsj (f:fs))] = and [testDsj [f], testCNF fs]
	testCNF (Dsj fs) = testDsj fs
	testCNF (Cnj fs) = and (map testCNF fs)
	
	testDsj :: [Form] -> Bool
	testDsj [(Prop x)] = True
	testDsj [(Neg (Prop x))] = True
	testDsj [(Cnj f)] = False
	testDsj [(Dsj (f:fs))] = testDsj [f] && testDsj fs
	testDsj (f:fs) = testDsj [f] && testDsj fs
	
	testAll :: Form -> Bool
	testAll f = (testAF f) && (testNNF f)
	
	{- when uncommented, this statement results in parse errors in Ex.4?
	testCNFMain :: Int -> IO ()
	testCNFMain n = do 
	fs <- getRandomFs n
	test n testAll (map convertCNF fs)
	-}
	
-----------------------------------------
--Ex.4 Clause Form
		
	cnf2cls :: [Form] -> Clauses
	cnf2cls [] = []
	cnf2cls [Prop x] = [[x]]
	cnf2cls [Neg (Prop x)] = [[-1*x]]
	cnf2cls [(Dsj(f:fs))] = (cnf2cls [f]) ++ cnf2cls fs
	cnf2cls [(Cnj(f:fs))] = (cnf2cls [f]) ++ (cnf2cls fs)
	
	testClause = Cnj[(Neg q), (Dsj[(Neg p), q])]