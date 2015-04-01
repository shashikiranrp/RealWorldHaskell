module Main where

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith predicate xs = filter ((/=0) . length) $ let (pre, nxt) = break predicate xs in [pre]  ++ (splitWith predicate (if null nxt then [] else tail nxt)) 

transpose :: [String] -> IO ()
transpose [] = return ()
transpose lines = putStrLn (map head lines) >> transpose (filter ((/=0) . length) $ map (\x -> if null x then [] else tail x) lines)

main :: IO ()
--main = getLine >>= return . head . words >>= putStrLn >> main
main = getContents >>= return . lines >>= transpose
