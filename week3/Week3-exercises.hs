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

	cnf :: Form -> Form
	cnf (Neg (Prop x)) = Neg (Prop x)
	cnf (Prop x) = Prop x
	cnf (Cnj fs) = Cnj(map cnf fs)
	cnf (Dsj [f1,f2]) = dist (cnf f1) (cnf f2)

	--is this correct??
	dist :: Form -> Form -> Form
	dist (Cnj[f1,f2]) fs = Cnj([(dist f1 fs),(dist f2 fs)])
	dist fs (Cnj[f1,f2]) = Cnj([(dist fs f1),(dist fs f2)])
	dist f1 f2 = Dsj([f1,f2])
		
	convertCNF :: Form -> Form
	convertCNF f = cnf (nnf (arrowfree (f)))
	
	test1 = (Neg (Neg (Neg (p))))
	test2 = (Neg (Dsj([p, (Neg q)])))
	test3 = (Neg (Cnj([Neg(p), Neg(q)])))
	test4 = Equiv (Impl p q) (Impl (Neg q) (Neg p))