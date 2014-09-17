module Main where
	
	import Week3

	main :: IO ()
	main = return ()

	contradiction :: Form -> Bool
	contradiction f = not (satisfiable f)

	tautology :: Form -> Bool
	tautology f = all (\ v -> eval v f) (allVals f)

	testTautology :: Bool
	testTautology = tautology (parseSingle "+(1 -1)")

	---- logical entailment
	entails :: Form -> Form -> Bool
	entails _ _ = True

	testEntails :: Bool
	testEntails = (testEntails1) && (testEntails2) && (testEntails3)

	-- {p} |# (p ∧ q)
	testEntails1 :: Bool
	testEntails1 = not (entails (parseSingle "1") (parseSingle "*(1 2)"))

	-- {p, q} |= (p ∧ q)
	testEntails2 :: Bool
	testEntails2 = entails (parseSingle "(1 2)") (parseSingle "*(1 2)")

	-- {p} |= (p ∨ q)
	testEntails3 :: Bool
	testEntails3 = entails (parseSingle "(1)") (parseSingle "+(1 2)")

	---- logical equivalence
	--equiv :: Form -> Form -> Bool

	parseSingle :: String -> Form
	parseSingle s = head (parse s)