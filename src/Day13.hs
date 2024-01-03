module Day13 where

-- Expects the first list to be "backwards": the head of ys is on the left of
-- the map, the head of xs is on the right of the map.
isReflection :: Eq a => [a] -> [a] -> Bool
isReflection [] _ = True
isReflection _ [] = True
isReflection (x:xs) (y:ys)
  | x == y = isReflection xs ys
  | otherwise = False

findReflection :: Eq a => [a] -> Maybe Integer
findReflection = go 0 []
  where go :: Eq a => Integer -> [a] -> [a] -> Maybe Integer
        go _ _ [] = Nothing
        go i acc (x:xs)
          | isReflection (x:acc) xs = Just i
          | otherwise = go (i + 1) (x:acc) xs
