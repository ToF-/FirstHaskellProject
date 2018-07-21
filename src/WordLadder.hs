module WordLadder (adjacent, wordGraph, ladder, toList ) where

type Graph = [(String,[String])]
type Start  = String
type Target = String
type Words = [String]
type Edge = (String, String)
type Path = [Edge]

adjacent :: String -> String -> Bool
adjacent s t | length s /= length t = False
adjacent s t | s == t = False
adjacent (c:cs) (d:ds) | c /= d     = cs == ds
                       | otherwise = adjacent cs ds

wordGraph :: [String] -> Graph
wordGraph ws = map (\w -> (w,filter (w `adjacent`) ws)) ws

toList = id

ladder :: Words -> Start -> Target -> Words
ladder _ s t | s == t = []
ladder _ s t | length s /= length t = []
ladder ws s t = path (ladder' (wordGraph ws) t [(s,s)] []) t

path :: Path -> Target -> Words
path ps t = reverse $ path' ps t
    where
    path' p t = case t `lookup` p of
      Nothing -> []
      Just u  -> case u == t of
        True -> [t]
        False -> t : path' p u

ladder' :: Graph -> Target -> [Edge] -> [Edge] -> Path
ladder' g t [] vs = []
ladder' g t ((s,n):_) vs | t == s = ((s,n):vs)
ladder' g t ((s,r):ss) vs = case s `lookup` g of
    Nothing   -> []
    Just []   -> ladder' g t ss ((s,r):vs)
    Just ns -> ladder' g t (ss ++ [(x,s) | x <- ns , x `lookup` vs == Nothing]) ((s,r):vs)
