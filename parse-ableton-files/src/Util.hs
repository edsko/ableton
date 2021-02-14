module Util (
    repeatedly
  , splitOn
  , splitFst
  , mergeAdjacent
  ) where

import Data.List (foldl')

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . foldl' . flip

-- | Like 'groupOn', but providing evidence of the grouping
--
-- Example:
--
-- >    groupSplit id [ (True, 'a'), (True, 'b'), (False, 'c'), (True, 'd') ]
-- > == [ (True, "ab"), (False, "c"), (True, "d") ]
splitOn :: forall x a b. Eq a => (x -> (a, b)) -> [x] -> [(a, [b])]
splitOn f = \case
    []   -> []
    x:xs -> let (a, b) = f x in go a [b] xs
  where
    go :: a -> [b] -> [x] -> [(a, [b])]
    go a bs []     = [(a, reverse bs)]
    go a bs (x:xs) = let (a', b) = f x in
                     if a == a' then                   go a  (b:bs) xs
                                else (a, reverse bs) : go a' [b]    xs

-- | Specialization of 'splitOn'
splitFst :: Eq a => [(a, b)] -> [(a, [b])]
splitFst = splitOn id

-- | Merge values for adjacent identical keys
mergeAdjacent :: (Eq a, Semigroup b) => [(a, b)] -> [(a, b)]
mergeAdjacent []                        = []
mergeAdjacent [(a, b)]                  = [(a, b)]
mergeAdjacent ((a, b) : (a', b') : rest)
  | a == a'   = mergeAdjacent ((a, b <> b') : rest)
  | otherwise = (a, b) : mergeAdjacent ((a', b') : rest)
