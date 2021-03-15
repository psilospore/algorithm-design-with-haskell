{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DataKinds #-}
module ChapterOne where

import Debug.Trace

-- | Permutations
perms_1_1 :: Show a => [a] -> [[a]]
perms_1_1 [] = [[]]
perms_1_1 (x:xs) = [zs | ys <- perms_1_1 xs, zs <- inserts_1 x ys ]

-- | Helper for Permutations. Given an element insert it in all the possible ways it can be inserted.
-- e.g. inserts_1 c [a, b] === [a, b, c] [a, c, b] [c, a, b]
inserts_1_1 :: a -> [a] -> [[a]]
inserts_1_1 x [] = [[x]]
inserts_1_1 x (y:ys) = (x:y:ys):map (y:) (inserts_1_1 x ys)

{-|
perms [a, b, c]

perms [c]

ys: [[]]
zs: inserts_1 c [] == [[c]]


perms [b, c]

ys: [[c]]
zs: inserts b [[c]] == [[b, c], [c, b]]

perms [a, b, c]

ys: perms [b, c] == [[b, c], [c, b]]
zs: inserts a [[b, c], [c, b]] == 

-}


-- | Permutations
perms_1 :: Show a => [a] -> [[a]]
perms_1 [] = [[]]
perms_1 (x:xs) = [zs | ys <- snd $ traceShowId ("ys", perms_1 xs), zs <- snd $ traceShowId ("zs:", inserts_1 x ys) ]





-- | Helper for Permutations. Given an element insert it in all the possible ways it can be inserted.
-- e.g. inserts_1 c [a, b] === [a, b, c] [a, c, b] [c, a, b]
inserts_1 :: Show a => a -> [a] -> [[a]]
inserts_1 x [] = [[x]]
inserts_1 x (y:ys) = let (_, _, _, result) = traceShowId ("inserts_1", (x, y:ys), "Result", (x:y:ys):map (y:) (inserts_1 x ys))
    in result

-- perms_1_2 :: [a] -> [[a]]
-- perms_1_2 = foldr (\x acc -> acc ++ inserts_1 x acc) [[]]


perms_1_3 :: [a] -> [[a]]
perms_1_3 = foldr (\x acc -> concatMap (inserts_1_1 x) acc) [[]]

-- Recursive defination

-- Picks out all possible combination from list. e.g. picks [1, 2] === [(1, [2]), (2, [1])]
picks :: [a] -> [(a, [a])]
picks [] = []
picks (x:xs) = (x, xs):[(y, x:ys)|(y, ys) <- picks xs]

