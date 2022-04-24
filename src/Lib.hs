module Lib
where

import Data.List (sort)

average :: [Int] -> Maybe Int
average [] = Nothing
average xs = Just $ sum xs `div` (length xs)

median :: (Integral a) => [a] -> Maybe Double
median [] = Nothing
median xs =
  let len = length xs
      xs' = sort xs
  in
    if odd len
      then Just . fromIntegral $ xs !! (len `div` 2)
      else Just $
        let median1 = xs !! (len `div` 2 - 1)
            median2 = xs !! (len `div` 2)
        in fromIntegral (median1 + median2) / 2

unique :: Eq a => [a] -> [a]
unique = foldr (\x l -> if x `elem` l then l else x : l) []