{-|

  Haskell-Learning #5

  Exercises
    Jan 22, 2013   Morita Yasuaki

-}

module Exercise where


-- |
-- >>> applyNtimes 3 (+3) 4
-- 13
-- >>> applyNtimes 5 (*2) 1
-- 32
-- >>> applyNtimes 5 ("HAHA " ++) "HEY"
-- "HAHA HAHA HAHA HAHA HAHA HEY"
-- >>> applyNtimes 10 ("ORA " ++) "URRRRYYYYY!"
-- "ORA ORA ORA ORA ORA ORA ORA ORA ORA ORA URRRRYYYYY!"

applyNtimes :: Int -> (b -> b) -> b -> b
applyNtimes n f b = iterate f b !! n


-- |
-- >>> concat' [[1,2],[3],[],[4,5]]
-- [1,2,3,4,5]
-- >>> concat' ["a","bc","d"]
-- "abcd"
-- >>> concat' [[2,3],[],[1],[4,5,6]]
-- [2,3,1,4,5,6]

concat' :: [[a]] -> [a]
concat' = foldr (++) []


-- [x0,x1,x2,...] = [0,1,(x0+x1),(x1+x2),...]
fibs  = 0:1:zipWith (+) fibs (tail fibs)

-- It's difficult!
-- [x0,x1,x2,...] = [0,1+x0,x0+x1,x1+x2,...]
fibs' = 0:scanl (+) 1 fibs'


-- |
-- >>> count (==1) [1,2,1,3,4,1,5,1]
-- 4
-- >>> count (\x -> x `mod` 2 == 0) [1,2,1,3,4,1,5,1]
-- 2
-- >>> count id [True,False,True,True]
-- 3

count p = length . filter p


-- |
-- >>> insert 61 [0,10,20,30,40,50,60,70,80,90]
-- [0,10,20,30,40,50,60,61,70,80,90]
-- >>> insert 27 [0,10,20,30,20,10,0,10,20,30]
-- [0,10,20,27,30,20,10,0,10,20,30]

insert :: Ord a => a -> [a] -> [a]
insert e (x:xs) | e >= x  = x:insert e xs
                | otherwise = e:x:xs
insert e _ = [e]


-- `map` by list comprehension.
-- |
-- prop> map (+1) xs == map' (+1) xs
--         where types = xs :: Int
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]


-- `map` by recursion.
-- |
-- prop> map (+1) xs == map'' (+1) xs
map'' :: (a -> b) -> [a] -> [b]
map'' f (x:xs) = f x : map'' f xs
map'' f []   = []


-- `map` by higher order functions.
-- |
-- prop> map (+1) xs == map''' (+1) xs
map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr ((:) . f) []

