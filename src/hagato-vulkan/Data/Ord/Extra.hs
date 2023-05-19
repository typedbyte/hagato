module Data.Ord.Extra (sortByM) where

sortByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM _ [] = pure []
sortByM comp (pivot:xs) = do
  (less, more) <- partitionM (\x -> fmap (== LT) $ comp x pivot) xs
  sortedLess <- sortByM comp less
  sortedMore <- sortByM comp more
  pure $ sortedLess ++ pivot:sortedMore

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
  isTrue <- f x
  (trues, falses) <- partitionM f xs
  if isTrue then
    pure (x:trues, falses)
  else
    pure (trues, x:falses)
