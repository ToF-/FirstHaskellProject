module WordLadder (adjacent, wordGraph, ladder, toList ) where

type Graph = [(String,[String])]
type Start  = String
type Target = String
type Words = [String]
type Edge = (String, String)
type Path = [Edge]

-- two words are adjacent if they differ only by one letter
adjacent :: String -> String -> Bool
adjacent s t | length s /= length t = False
adjacent s t | s == t = False
adjacent (c:cs) (d:ds) | c /= d     = cs == ds
                       | otherwise = adjacent cs ds

-- create a graph of adjacent words
wordGraph :: [String] -> Graph
wordGraph ws = map (\w -> (w,filter (w `adjacent`) ws)) ws

toList = id

-- find the ladder between two words
ladder :: Words -> Start -> Target -> Words
ladder _ start target | start == target = []
ladder _ start target | length start /= length target = []
ladder words start target = walk target (ladder' (wordGraph words) target [(start,start)] [])

-- climb down the path from a target word to the starting word (where word == parent)
walk :: Target -> Path -> Words
walk target path = reverse (walk' target path)
    where
    walk' :: Target -> Path -> Words
    walk' target path = case target `lookup` path of
      Nothing -> []
      Just parent  -> case parent == target of
        True -> [target]
        False -> target : walk' parent path

ladder' :: Graph -> Target -> [Edge] -> [Edge] -> Path
ladder' g t [] vs = []
ladder' g t ((s,n):_) vs | t == s = ((s,n):vs)
ladder' g t ((s,r):ss) vs = case s `lookup` g of
    Nothing   -> []
    Just []   -> ladder' g t ss ((s,r):vs)
    Just ns -> ladder' g t (ss ++ [(x,s) | x <- ns , x `lookup` vs == Nothing]) ((s,r):vs)
