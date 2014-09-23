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
		postconditions: output is in CNF
	-}
	cnf :: Form -> Form
	cnf (Prop x) = Prop x
	cnf (Neg (Prop x)) = Neg (Prop x)
	cnf (Cnj fs) = Cnj(map cnf fs)
	cnf (Dsj []) = Dsj[] --distList cannot handle empty list
	cnf (Dsj fs) = distList (map cnf fs)

	{-
		apply distribution laws on list of forms
		precondition: input is in cnf
		postcondition: outputs are cnf with distribution laws applied on conjunctions
	-}
	distList :: [Form] -> Form
	distList [f] = f
	distList (f:fs) = dist f (distList fs)
	
	{-
		precondition: two inputs are in cnf
		postcondition: distribution laws applied on inputs
	-}
	dist :: Form -> Form -> Form
	dist (Cnj f1) f2 = Cnj(map (\f -> (dist f f2)) f1)
	dist f1 (Cnj f2) = Cnj(map (\f -> (dist f1 f)) f2)
	dist f1 f2 = Dsj[f1, f2]
	
	{-
		main conversion function
	-}
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
	
	{-	Results of convertCNF on test1-test7
		1. -1
		2. *(-1 2)
		3. +(1 2)
		4. *(*(*(+(+(-1 2) 1) +(+(-1 2) -2)) *(+(+(-1 2) -2) +(+(-1 2) 1))) *(*(+(+(2 -1) 1) +(+(2 -1) -2))  *(+(+(2 -1) -2) +(+(2 -1) 1))))
		5. *(+(1 3) +(2 3))
		6. *(+(1 2) +(1 3))
		7. +(+(1 2) 1)
	-}
	
	
-------------------------------------------
--Ex.3 CNF test (3 hours)
	
	testCNF :: Form -> Bool
	testCNF (Prop x) = True
	testCNF (Neg (Prop x)) = True		
	testCNF (Neg f) = False	 				--NNF		
	testCNF (Cnj fs) = and (map testCNF fs)
	testCNF (Dsj fs) = testDsj fs && and (map testCNF fs) 
	testCNF (Impl f1 f2) = False			--arrowfree
	testCNF (Equiv f1 f2) = False			--arrowfree
	
	testDsj :: [Form] -> Bool
	testDsj [(Cnj f)] = False
	testDsj ((Cnj f):_) = False
	testDsj [(Dsj (f:fs))] = testDsj [f] && testDsj fs
	testDsj ((Dsj (f:fs)):gs) = testDsj [f] && testDsj fs && testDsj gs
	testDsj _ = True --anything else then conjunction / disjunction is True
	
	{-
		testCNF is tested using the following statement:
		> testCNFMain 100
		> results in 100 tests passed
		
		testCNFMain is defined below
	-}
		
-----------------------------------------
--Ex.4 Clause Form (1 hour)
		
	cnf2cls :: Form -> Clauses
	cnf2cls (Cnj fs) = (map clsDsj fs)
	cnf2cls f = [[clsProp f]]
	
	clsDsj :: Form -> Clause
	clsDsj (Dsj fs) = (map clsProp fs)
	clsDsj f = [clsProp f]
	
	clsProp :: Form -> Int
	clsProp (Prop x) = -x
	clsProp (Neg (Prop x)) = -1*x
	
	testClause1 = Cnj[(Neg q), (Dsj[(Neg p), q])]
	-- [[-2],[-1],[2]]
	testClause2 = Cnj[(Neg q), (Dsj[(Neg p), q, (Neg r)])]
	
	--To Be Continued..	
	clsTest' :: Form -> Clauses -> Bool
	clsTest' (Cnj (f:fs)) (c:cs) = match f c
	
	match :: Form -> Clause -> Bool
	match (Prop x) [c] = x == c
	match (Neg (Prop x)) [c] = x == -1*c
	match (Dsj (f:fs)) (c:cs) = match' f c
	
	match' :: Form -> Int -> Bool
	match' (Prop x) c = x == c
	match' (Neg (Prop x)) c = x == -1*c
	
------------------------------------------
--Main test method for CNF

	testCNFMain :: Int -> IO ()
	testCNFMain n = do 
	fs <- getRandomFs n
	test n testCNF (map convertCNF fs)