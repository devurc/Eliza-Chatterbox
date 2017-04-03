module Main where
  
    import Test.HUnit
    import Debug.Trace

    substitute :: Eq a => a -> [a] -> [a] -> [a]
    substitute _ [] _ = []
    substitute wildcard (t:tail) s
        | wildcard == t    = (s) ++ (substitute wildcard tail s)
        | otherwise = t: (substitute wildcard tail s)

    singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
    singleWildcardMatch (wc:ps) (x:xs)
        -- Remove all instances of wc, don't think its necessary
        | ps == [x | x <- xs, x /= wc] = Just [x]
        -- | ps == xs = Just [x]
        | otherwise = Nothing

    -- TO DO -- 
    -- longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
    -- longerWildcardMatch (wc:ps) (x:xs)
    --     | ps == xs  = Nothing
    --     | otherwise = Just x : longerWildcardMatch ps xs


    match :: Eq a => a -> [a] -> [a] -> Maybe [a]
    match _[][] = Just []
    match _[]_  = Nothing
    match _ _[] = Nothing
    match wildcard (p:ps) (s:ss)
        -- Case when lists are equal
        | (p:ps) == (s:ss)             = Just []
        -- Case when wilcard is not in pattern (p:ps)
        | not $ wildcard `elem` (p:ps) = Nothing
        -- Case when wildcard is not the first element of p
        -- and head elemnts are equal
        | wildcard /= p && p == s      = match wildcard ps ss 
        -- Case when the first element is the wildcard
        | wildcard == p                = singleWildcardMatch (p:ps) (s:ss)
        | otherwise                    = Nothing

    substituteTest =
        test [
            substitute 'x' "3*cos(x) + 4 - x" "5.37" ~?= "3*cos(5.37) + 4 - 5.37"
        ]

    matchTest =
        test [
            -- Empty list edge cases
            match '*' "" "" ~?= Just [],              -- 0 
            match '*' "abba" "" ~?= Nothing,          -- 1
            match '*' "" "abba" ~?= Nothing,          -- 2
            -- singleWildCardMatch cases
            match '*' "*do" "bdo" ~?= Just "b",       -- 3
            match '*' "a" "a" ~?= Just [],            -- 4
            match '*' "*" "a" ~?= Just "a",           -- 5
            match '*' "*X*" "aXb" ~?= Just "a",       -- 6 (FAILING)
            match 'x' "2*x+3" "2*7+3" ~?= Just "7",   -- 7
            match '*' "frodo" "gandalf" ~?= Nothing,  -- 8 
            match 2 [1,3..5] [1,3..5] ~?= Just []     -- 9
            -- longerWildCardMatch cases
            -- match '*' "* and *" "you and me" ~?= Just "you",
            -- match 'x' "2*x+3+x" "2*7+3" ~?= Nothing,
            -- match '*' "*do" "dobedo" ~?= Just "dobe",
            -- match '*' "*do" "bedobe" ~?= Nothing,
            -- match '*' "*" "abba" ~?= Just "abba",
            -- match '*' "*X*" "aaXbb" ~?= Just "aa"
        ]

    main = runTestTT $
        test [
            "substitute" ~: Main.substituteTest, -- ALL OK -- 
            "match" ~: Main.matchTest
        -- "transformationApply" ~: transformationApplyTest,
        -- "transformationsApply" ~: transformationsApplyTest,
        -- "reflect" ~: reflectTest,
        -- "rulesApply" ~: rulesApplyTest
        ]