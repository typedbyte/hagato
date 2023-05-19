module Data.List.Extra (pairs) where

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:ys) = fmap (\y -> (x,y)) ys ++ pairs ys