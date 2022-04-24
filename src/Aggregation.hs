module Aggregation
where

import Data.List (sort)
import Data.Char (isDigit)
import System.IO (openFile, IOMode(ReadMode), hClose, hGetContents)
import Control.Exception (bracket)
import Control.Monad (forM_)

aggregate :: IO ()
aggregate = do
  bracket (openFile "assets/nums" ReadMode) hClose $ \h -> do
    cs <- hGetContents h
    let ns = map read . filter isNumber . lines $ cs
    case ns of
      [] -> putStrLn "No number in assets/nums"
      _  -> sequence_
        [ putStrLn $ "max: " ++ show (maximum ns)
        , putStrLn $ "min: " ++ show (minimum ns)
        , putStrLn $ "avg: " ++ show (average ns)
        , putStrLn $ "max: " ++ show (maximum ns)
        ]
    return ()


isNumber :: String -> Bool
isNumber ""        = False
isNumber "0"       = True
isNumber ('0' : _) = False
isNumber cs        = all isDigit cs

average :: [Int] -> Maybe Int
average [] = Nothing
average xs = Just $ sum xs `div` (length xs)

median :: (Integral a) => [a] -> Maybe Double
median [] = Nothing
median xs =
  let xs' = sort . unique $ xs
      len = length xs'
  in
    if odd len
      then Just . fromIntegral $ xs' !! (len `div` 2)
      else Just $
        let median1 = xs' !! (len `div` 2 - 1)
            median2 = xs' !! (len `div` 2)
        in fromIntegral (median1 + median2) / 2

unique :: Eq a => [a] -> [a]
unique = foldr (\x l -> if x `elem` l then l else x : l) []