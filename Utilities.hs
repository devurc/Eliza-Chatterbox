module Utilities where


map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- mapping of Maybe variables
-- if nothing, return nothing
-- if there is something, apply function to it
-- Just is required whenever nothing is a possibility
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)


orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a


try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)


fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- takes length of array and turns it into a Num
-- multiplies length by arg u and takes the integer floor
-- takes the list element of resulting integer index 
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

