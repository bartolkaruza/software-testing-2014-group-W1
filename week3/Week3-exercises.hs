module Main where
	
	import Week3

	main :: IO ()
	main = return ()

	-- Ex1

	contradiction :: Form -> Bool
	contradiction f = not (satisfiable f)

	tautology :: Form -> Bool
	tautology f = all (\ v -> eval v f) (allVals f)

	testTautology :: Bool
	testTautology = tautology (parseSingle "+(1 -1)")

	---- logical entailment
	entails :: Form -> Form -> Bool
	entails f j = tautology (Impl f j)

	testEntails :: Bool
	testEntails = (testEntails1) && (testEntails2) && (testEntails3)

	-- {p} |# (p ∧ q)
	testEntails1 :: Bool
	testEntails1 = not (entails (parseSingle "1") (parseSingle "*(1 2)"))

	-- {p, q} |= (p ∧ q)
	testEntails2 :: Bool
	testEntails2 = entails (parseSingle "*(1 2)") (parseSingle "*(1 2)")

	-- {p} |= (p ∨ q)
	testEntails3 :: Bool
	testEntails3 = entails (parseSingle "1") (parseSingle "+(1 2)")

	---- logical equivalence
	equiv :: Form -> Form -> Bool
	equiv f j = tautology (Equiv f j)

	testEquiv1 :: Bool
	testEquiv1 = equiv (parseSingle "1") (parseSingle "1")

	testEquiv2 :: Bool
	testEquiv2 = not (equiv (parseSingle "1") (parseSingle "-1"))

	parseSingle :: String -> Form
	parseSingle s = head (parse s)

	-- Ex 2

	prepareCnf :: Form -> Form
	prepareCnf f = nnf (arrowfree f)

	cnf :: Form -> Form
	cnf (Prop x) = Prop x
	cnf (Neg (Prop x)) = (Neg (Prop x))
	cnf (Cnj [x,y]) = Cnj [(cnf x),(cnf y)]
	cnf (Dsj [x,y]) = dist (cnf x) (cnf y)

	dist :: Form -> Form -> Form
	dist (Cnj [x,y]) z = Cnj [(dist x z),(dist y z)]
	dist x (Cnj [y,z]) = Cnj [(dist x y),(dist x z)]
	dist x y = Dsj [x,y] 

	-- Ex 3
	isCnf :: Form -> Bool
	isCnf (Prop x) = True
	isCnf (Neg (Prop x)) = True
	isCnf (Neg _) = False	-- Negation not of a property
	isCnf (Dsj ((Cnj x):xs)) = False -- Conjunction below a Disjunction
	isCnf (Dsj (x:xs)) = isCnf x && isCnf(Dsj xs)
	isCnf (Dsj fs) = foldr (\x y -> (isCnf x) && y) True fs
	isCnf (Cnj fs) = foldr (\x y -> (isCnf x) && y) True fs
	isCnf (Impl _ _ ) = False
	isCnf (Equiv _ _ ) = False

	testCnf :: Int -> IO ()
	testCnf n = do
		fs <- getRandomFs n
		test n isCnf (map cnf (map prepareCnf fs))
	--dist :: Form -> Form -> Form
