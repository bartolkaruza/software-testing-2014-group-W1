module Main where
	
	import Week3

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

	--preconditions: input is in NNF
	cnf :: Form -> Form
	cnf (Prop x) = Prop x
	cnf (Neg (Prop x)) = Neg (Prop x)
	cnf (Cnj fs) = Cnj(map cnf fs)
	cnf (Dsj []) = Dsj[]				--distList cannot handle empty list
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
	
	--precondition: input is in cnf
	simplify :: Form -> Form
	simplify 
			
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

	testCNFMain :: Int -> IO ()
	testFormsOnCNF n = do 
	fs <- getRandomFs n
	test n testComb (map convertCNF fs)

	testComb :: Form -> Bool
	testComb f = (testAF f) && (testNNF f)

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
	
	{- 
	we should test each disjunction within a conjunction, however the output of convertCNF contains nested conjunctions/disjunctions --> we need to simplify the CNF result first...
	-}