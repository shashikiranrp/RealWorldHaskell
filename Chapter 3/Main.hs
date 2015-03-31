module Main where

import Data.List(sortBy)

data Point = Point {x::Int, y::Int} deriving(Show)
data AngleDirection = LeftSide | RightSide | Colinear deriving (Eq, Show)

data Ratio = Ratio {n :: Int, d :: Int}
            | Inf deriving(Show)

instance Eq Ratio where
  Inf == Inf = True -- math is not happy here, but OK for now. 
  Inf == _ = False
  _ == Inf = False
  Ratio n1 d1 == Ratio n2 d2 = ((fromIntegral(n1) / gcd1) == (fromIntegral(n2) / gcd2)) && ((fromIntegral(d1) / gcd1) == (fromIntegral(d2) / gcd2))
                where gcd1 = fromIntegral $ gcd n1 d1 
                      gcd2 = fromIntegral $ gcd n2 d2

ratioAsFloat :: Ratio -> Float
ratioAsFloat r = (fromIntegral $ n r) / (fromIntegral $ d r)

getSlope :: Point -> Point -> Ratio
getSlope p1 p2  | (x p2 - x p1) == 0 = Inf
                | otherwise = Ratio (y p2 - y p1) (x p2 - x p1)

isColinear :: Point -> Point -> Point -> Bool
isColinear p1 p2 p3 = getSlope p1 p2 == getSlope p2 p3 

getAngle :: Ratio -> Float
getAngle r1 = let floatRatio = ratioAsFloat r1 in if isInfinite floatRatio then 90.0 else (atan floatRatio) * 180 / pi

angleBetweenSlopes :: Ratio -> Ratio -> Float
angleBetweenSlopes r1 r2 = getAngle r1 - getAngle r2 

getDirection :: Point -> Point -> Point -> AngleDirection
getDirection  p1 p2 p3 
              | isColinear p1 p2 p3 = Colinear
              | angleDiff < 0 = RightSide  
              | otherwise = LeftSide 
              where angleDiff = getAngle m1 - getAngle m2 
                              where m1 = getSlope p1 p2
                                    m2 = getSlope p2 p3

data Tree a = Empty 
              | Node a (Tree a) (Tree a)
              deriving(Show)

maxHeight :: Tree a -> Int
maxHeight = maxHeight' 0

maxHeight' :: Int -> Tree a -> Int
maxHeight' currentHeight t = case t of 
                              Empty -> currentHeight
                              Node _ l r -> max (maxHeight' (currentHeight + 1) l) (maxHeight' (currentHeight + 1) r)

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

avg :: [Int] -> Double
avg xs = fromIntegral (sum xs) / fromIntegral (len xs)

palin :: [a] -> [a]
palin [] = []
palin xs = xs ++ reverse xs

isPalin :: Eq a => [a] -> Bool
isPalin [] = True
isPalin [_] = True
isPalin (x:xs)
      | x == (last xs) = isPalin (init xs) 
      | otherwise = False

sortByLen :: [[a]] -> [[a]]
sortByLen xs = sortBy compareLen xs 
  where compareLen xs ys
          | len xs < len ys = LT
          | len xs > len ys = GT
          | otherwise = EQ

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse ise xs'@(x:xs) 
  | null xs = x
  | otherwise = x ++ [ise] ++ (intersperse ise xs)

main :: IO ()
main = getLine >>= putStrLn
