module WordLadder (adjacent, wordGraph, toList ) where

type WordGraph = [(String,[String])]

adjacent :: String -> String -> Bool
adjacent s t | length s /= length t = False
adjacent s t | s == t = False
adjacent (c:cs) (d:ds) | c /= d     = cs == ds
                       | otherwise = adjacent cs ds

wordGraph :: [String] -> WordGraph
wordGraph ws = map (\w -> (w,filter (w `adjacent`) ws)) ws

toList = id
