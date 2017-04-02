singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
    | ps == xs = Just [x]
    | otherwise = Nothing

-- "*do" "dobedo"
longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (wc:ps) (x:xs)
    | ps == xs  = Nothing
    | otherwise = Just x : longerWildcardMatch ps xs


match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _[]_  = Nothing
match _ _[] = Nothing
match wildcard (p:ps) (s:ss)
   | wildcard /= p = Nothing 
   | wildcard == p = singleWildcardMatch (p:ps) (s:ss)
   | otherwise     = Nothing

   -- matchTest =
   --  test [
   --    match 'x' "2*x+3" "2*7+3" ~?= Just "7",
   --    match '*' "frodo" "gandalf" ~?= Nothing,
   --    match 2 [1,3..5] [1,3..5] ~?= Just [],
   --    match '*' "* and *" "you and me" ~?= Just "you",
   --    match 'x' "2*x+3+x" "2*7+3" ~?= Nothing,
   --    match '*' "*do" "bdo" ~?= Just "b",
   --    match '*' "*do" "dobedo" ~?= Just "dobe",
   --    match '*' "*do" "bedobe" ~?= Nothing,
   --    match '*' "" "" ~?= Just [],
   --    match '*' "abba" "" ~?= Nothing,
   --    match '*' "" "abba" ~?= Nothing,
   --    match '*' "a" "a" ~?= Just [],
   --    match '*' "*" "a" ~?= Just "a",
   --    match '*' "*" "abba" ~?= Just "abba",
   --    match '*' "*X*" "aXb" ~?= Just "a",
   --    match '*' "*X*" "aaXbb" ~?= Just "aa"
   --  ]