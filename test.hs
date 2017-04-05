main = 
    putStrLn "hello world!\n"
    

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100
                        then x
                        else x*2) + 1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x ]

length' xs = sum [1 | _ <- xs ]

lengthRecurse :: (Num b) => [a] -> b
lengthRecurse [] = 0
-- how does lengthRecurse xs become the tail of the original xs?
lengthRecurse (_:xs) = 1 + lengthRecurse xs

fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

last' :: [a] -> a
last' [] = error "empty list"
-- has incorrect output for list of size 1, so adding single element list case
last' (x:[]) = x
last' (x:xs) = xs !! (length' xs - 1)

init' :: [a] -> [a]
init' [] = error "empty list"
-- init' (x:xs) = [ x | x <- xs, ]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b



