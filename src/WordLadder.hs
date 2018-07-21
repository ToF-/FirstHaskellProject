module WordLadder (adjacent, wordGraph, ladder, toList ) where

type WordGraph = [(String,[String])]

adjacent :: String -> String -> Bool
adjacent s t | length s /= length t = False
adjacent s t | s == t = False
adjacent (c:cs) (d:ds) | c /= d     = cs == ds
                       | otherwise = adjacent cs ds

wordGraph :: [String] -> WordGraph
wordGraph ws = map (\w -> (w,filter (w `adjacent`) ws)) ws

toList = id

ladder :: [String] -> String -> String -> [String]
ladder _ s t | length s /= length t = []
ladder ws s t = ladder' (wordGraph ws) s t

ladder' :: WordGraph -> String -> String -> [String]
ladder' g s t = case fmap (t `elem`) (s `lookup` g) of
    Nothing   -> []
    Just True -> [s,t]
    Just False -> []
