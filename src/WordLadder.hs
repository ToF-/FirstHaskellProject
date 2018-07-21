module WordLadder (adjacent ) where

adjacent :: String -> String -> Bool
adjacent s t | length s /= length t = False
adjacent s t | s == t = False
adjacent (c:cs) (d:ds) | c /= d     = cs == ds
                       | otherwise = adjacent cs ds

