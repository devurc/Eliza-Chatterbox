module Main where

    import Test.HUnit
    import Debug.Trace
    import Utilities

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
        -- Case when the first element of the lists match
        -- and the lengths match (e.g. match '*' "*X*" "aXb")
        -- returns Just "a"
        | length ps == length xs && ps !! 0 == xs !! 0 = Just [x]
        | otherwise = Nothing

    -- TO DO --
    longerWildcardMatch :: (Show a, Eq a) => [a] -> [a] -> Maybe [a]
    longerWildcardMatch (wc:ps) (x:xs)
         -- If the two list tails are the same length that means that
         -- the wildcard is singular. Therefore this function should
         -- return nothing
         | ps == xs  = Nothing
         -- Else just keep collecting all the letters that come
         -- before ps e.g we are extracting 'dobe' from 'dobedo' where
         -- wc:ps == *do. 
         | otherwise = 
            (   
                trace "before call: " 
                traceShow xs
                mmap (x:) $ match wc (wc:ps) xs
            )

    match :: (Show a, Eq a) => a -> [a] -> [a] -> Maybe [a]
    match _[][] = Just []
    match _[]_  = Nothing
    match _ _[] = Nothing
    match wildcard (p:ps) (s:ss)
        -- Case when lists are equal
        | (p:ps) == (s:ss)             = (
            trace "1st"
            Just [])
        -- Case when wildcard is not in pattern (p:ps)
        | not $ wildcard `elem` (p:ps) = (
            trace "2nd"
            Nothing)
        -- When (p:ps) consists of only the wildcard
        | wildcard == p && length (p:ps) == 1 = Just (s:ss)
        -- Case with multiple wildcards 
        | head ps == s && ps /= ss && length ps == length ss     = (
            trace "3rd"
            traceShow ps
            traceShow ss
            Just [])
        -- Case when wildcard is not the first element of p
        -- and head elemnts are equal
        | wildcard /= p && p == s      = (
            trace "4th"
            match wildcard ps ss
            )
        -- Case when the first element is the wildcard
        | wildcard == p                = if (singleWildcardMatch (p:ps) (s:ss) == Nothing)
                                            then (longerWildcardMatch (p:ps) (s:ss))
                                            else (singleWildcardMatch (p:ps) (s:ss))
        | otherwise                    = Nothing

    substituteTest =
        test [
            substitute 'x' "3*cos(x) + 4 - x" "5.37" ~?= "3*cos(5.37) + 4 - 5.37"
        ]

    matchTest =
        test [
            -- Empty list edge cases
            match '*' "" "" ~?= Just []              
            ,match '*' "abba" "" ~?= Nothing          
            ,match '*' "" "abba" ~?= Nothing          
            -- -- singleWildCardMatch cases
            ,match '*' "*do" "bdo" ~?= Just "b"       
            ,match '*' "a" "a" ~?= Just []            
            ,match '*' "*" "a" ~?= Just "a"           
            ,match '*' "*X*" "aXb" ~?= Just "a"       
            ,match 'x' "2*x+3" "2*7+3" ~?= Just "7"   
            ,match '*' "frodo" "gandalf" ~?= Nothing  
            ,match 2 [1,3..5] [1,3..5] ~?= Just []   
            -- longerWildCardMatch cases
            ,match '*' "* and *" "you and me" ~?= Just "you"
            ,match 'x' "2*x+3+x" "2*7+3" ~?= Nothing
            ,match '*' "*do" "dobedo" ~?= Just "dobe"
            ,match '*' "*do" "bedobe" ~?= Nothing
            ,match '*' "*" "abba" ~?= Just "abba"
            ,match '*' "*X*" "aaXbb" ~?= Just "aa"
        ]

    main = runTestTT $
        test [
            "substitute" ~: Main.substituteTest -- ALL OK --
            ,"match" ~: Main.matchTest
        -- "transformationApply" ~: transformationApplyTest,
        -- "transformationsApply" ~: transformationsApplyTest,
        -- "reflect" ~: reflectTest,
        -- "rulesApply" ~: rulesApplyTest
        ]
