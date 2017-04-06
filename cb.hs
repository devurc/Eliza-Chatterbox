module Main where

    import Test.HUnit
    import Debug.Trace
    import Utilities
    import Data.Maybe

    substitute :: Eq a => a -> [a] -> [a] -> [a]
    substitute _ [] _ = []
    substitute wildcard (t:tail) s
        | wildcard == t = (s) ++ (substitute wildcard tail s)
        | otherwise = t: (substitute wildcard tail s)

    singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
    singleWildcardMatch (wc:ps) (x:xs)
        -- Remove all instances of wc, don't think its necessary
        | ps == [x | x <- xs, x /= wc]                 = Just [x]
        -- | ps == xs = Just [x]
        -- Case when the first element of the lists match
        -- and the lengths match 
        | length ps == length xs && ps !! 0 == xs !! 0 = Just [x]
        | otherwise                                    = Nothing

    longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
    longerWildcardMatch (wc:ps) (x:xs)
         -- If the two list tails are the same length that means that
         -- the wildcard is singular.
         | ps == xs  = Nothing
         -- Else just keep collecting all the letters that come before ps
         | otherwise = mmap (x:) $ match wc (wc:ps) xs

    match :: Eq a => a -> [a] -> [a] -> Maybe [a]
    match _ [][] = Just []
    match _ [] _ = Nothing
    match _ _ [] = Nothing
    match wildcard (p:ps) (s:ss)
        -- Case when lists are equal
        | (p:ps) == (s:ss)                    = Just []
        -- Case when wildcard is not in pattern (p:ps)
        | not $ wildcard `elem` (p:ps)        = Nothing
        -- When (p:ps) consists of only the wildcard
        | wildcard == p && length (p:ps) == 1 = Just (s:ss)
        -- Case with multiple wildcards 
        | head ps == s && ps /= ss && length ps == length ss = Just []
        -- Case when wildcard is not the first element of p
        -- and head elemnts are equal
        | wildcard /= p && p == s            = match wildcard ps ss
        -- If wildcard is the first element of pattern
        | wildcard == p                      = orElse (singleWildcardMatch (p:ps) (s:ss))
                                                (longerWildcardMatch (p:ps) (s:ss))
        | otherwise                          = Nothing


    -------------------------------------------------------
    -- Applying patterns
    --------------------------------------------------------

    -- Applying a single pattern
    transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
    transformationApply _ _ [] _       = Nothing
    transformationApply _ _ _ ([],[])  = Nothing
    -- The second parameter to this function is another function which is used 
    -- to transform the result of the match before the substitution is made. 
    -- Currently does not use the second function argument
    transformationApply wildcard f orig present
        | match wildcard first orig /= Nothing = Just (substitute wildcard second matchResult)
        | otherwise = Nothing
        where first = fst(present)
              second = snd(present)
              matchResult = fromJust(match wildcard first orig)

    -- Applying a list of patterns until one succeeds
    transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
    transformationsApply _ _ [] _ = Nothing
    transformationsApply _ _ _ [] = Nothing
    transformationsApply wildcard f (pres:plist) orig =
        orElse (transformationApply wildcard f orig pres)
            (transformationsApply wildcard f plist orig) 


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

    frenchPresentation = ("My name is *", "Je m'appelle *")
  
    transformationApplyTest =
        test [
            transformationApply '*' id "My name is Zacharias" frenchPresentation
            ~?= Just "Je m'appelle Zacharias",
            transformationApply '*' id "My shoe size is 45" frenchPresentation
            ~?= Nothing
        ]

    swedishPresentation = ("My name is *", "Mitt namn är *")    
    presentations = [frenchPresentation, swedishPresentation]

    transformationsApplyTest =
        test [
            transformationsApply '*' id presentations "My name is Zacharias"
            ~?= Just "Je m'appelle Zacharias",
            transformationsApply '*' id (reverse presentations) "My name is Zacharias"
            ~?= Just "Mitt namn är Zacharias",
            transformationsApply '*' id (reverse presentations) "My shoe size is 45"
            ~?= Nothing
        ]

    main = runTestTT $
        test [
            "substitute" ~: Main.substituteTest -- ALL OK --
            ,"match" ~: Main.matchTest
            ,"transformationApply" ~: transformationApplyTest
            ,"transformationsApply" ~: transformationsApplyTest
            -- ,"reflect" ~: reflectTest
            -- "rulesApply" ~: rulesApplyTest
        ]
