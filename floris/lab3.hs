import Lecture3

-- Test vars
p = Prop 1
q = Prop 2
r = Prop 3

-- Exercise 1 (40 mins)
contradiction :: Form -> Bool
contradiction f = not $ any (\ v -> evl v f) (allVals f)

testContradiction =
    (contradiction $ Cnj $ [p, Neg p]) == True &&
    (contradiction $ Cnj $ [p, p]) == False

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

testTautology =
    (tautology $ Dsj $ [p, Neg p]) == True &&
    (tautology $ Impl p p) == True &&
    (tautology $ Impl p q) == False

-- | logical entailment 
entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

testEntails =
    (entails (Cnj [(Impl p q), (Impl q r)]) (Impl p r)) == True &&
    (entails (Cnj [(Impl p q), (Impl q r)]) (Impl r q)) == False

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b)

testEquiv =
    -- https://www.wikiwand.com/en/Boolean_algebra#/Laws
    (equiv (Dsj [p, (Dsj [q, r])]) (Dsj [r, (Dsj [p, q])])) == True &&
    (equiv (Cnj [p, (Cnj [q, r])]) (Cnj [r, (Cnj [p, q])])) == True &&
    (equiv (Dsj [p, (Cnj [q, r])]) (Cnj [(Dsj [p, q]), (Dsj [p, r])])) == True


-- Exercise 2: success cases
parseCases :: [String]
parseCases = ["*(1 +(2 -3))", "(1<=>2)"] -- etc
testParser = all (\ s -> let r = parse s in
                         (length r == 1) && (show (head r) == s)) parseCases