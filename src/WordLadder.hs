module WordLadder (adjacent, wordGraph, ladder, toList ) where

type Graph = [(String,[String])]
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
ladder :: [String] -> String -> String -> [String]
ladder _ start target | start == target = []
ladder _ start target | length start /= length target = []
ladder words start target = walk target (ladder' (wordGraph words) target [(start,start)] [] )
  where
  ladder' :: Graph -> String -> [Edge] -> [Edge] -> Path
  ladder' _ _ [] _ = []

  ladder' _ target (edge@(word,parent):_) visited
    | word == target = edge:visited

  ladder' graph target (edge@(word,_):edges) visited = case lookup word graph of
      Nothing   -> []
      Just neighbors -> ladder' graph target (edges ++ [ (neighbor,word)
                                                     | neighbor <- neighbors
                                                     , lookup neighbor visited == Nothing])
                                            (edge:visited)

-- climb the path from a target word to the starting word (where word == parent)
walk :: String -> Path -> [String]
walk target path = reverse (walk' target path)
    where
    walk' :: String -> Path -> [String]
    walk' target path = case lookup target path of
      Nothing -> []
      Just parent  -> case parent == target of
        True -> [target]
        False -> target : walk' parent path

