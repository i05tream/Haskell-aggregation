module Lib
where

average :: [Int] -> Maybe Int
average [] = Nothing
average xs = Just $ sum xs `div` (length xs)

median :: [Int] -> Maybe Int
median [] = Nothing
