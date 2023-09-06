import Data.List (nub)

fact :: Int -> Int
fact 1 = 1
fact x = fact_aux 1 x 
    where 
        fact_aux :: Int -> Int -> Int
        fact_aux acc y 
            | y == 1 = acc
            | otherwise = fact_aux (y * acc) (y - 1)


mygcd :: Int -> Int -> Int
mygcd a b = case b of
    0 -> a 
    _ -> mygcd b (a `mod` b)

mygcd1 :: Int -> Int -> Int
mygcd1 a b 
    | b == 0 = a 
    | otherwise = mygcd1 b (a `mod` b)

mySqrt :: Int -> Int
mySqrt a = mySqrtHelper a a
    where
        mySqrtHelper :: Int -> Int -> Int
        mySqrtHelper x y
            | y * y > x  = mySqrtHelper x (y - 1)
            | otherwise  = y

mymin :: [Int] -> Int
mymin [] = error "Empty list"
mymin [x] = x
mymin (x:xs) = min x (mymin xs)

mymax :: [Int] -> Int
mymax list = case list of
    [] -> error "Empty list"
    [x] -> x
    (x:xs) -> max x (mymax xs)


melem :: Int -> [Int] -> Bool
melem _ [] = False
melem a [x] = if a == x then True else False
melem a (x:xs) = if a == x then True else melem a xs 


-- unique :: [Int] -> [Int]
-- unique list = nub list

unique :: [Int] -> [Int]
unique [] = []
unique [a] = [a]
unique l = foldr (\e acc -> if elem e acc then acc else e : acc) [] l


fizzBuzz :: [Int] -> [String]
fizzBuzz = map fizzBuzzHelper
  where
    fizzBuzzHelper :: Int -> String
    fizzBuzzHelper n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod` 3 == 0 = "Fizz"
      | n `mod` 5 == 0 = "Buzz"
      | otherwise = show n


mymapl :: (a -> b) -> [a] -> [b]
mymapl f = foldr (\x acc -> f x : acc) []

mymapr :: (a -> b) -> [a] -> [b]
mymapr f = foldr (\x acc -> f x : acc) []

myfilterl :: (a -> Bool) -> [a] -> [a]
myfilterl p = foldl (\acc x -> if p x then acc ++ [x] else acc) []

myfilterr :: (a -> Bool) -> [a] -> [a]
myfilterr p = foldr (\x acc -> if p x then x : acc else acc) []

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f z xs = foldr (\x acc -> f acc x) z (reverse xs)

bubbleSort :: [Int] -> [Int]
bubbleSort = foldr bubble []
  where
    bubble x [] = [x]
    bubble x (y:ys)
      | x <= y = x : y : ys
      | otherwise = y : bubble x ys

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) =
  let smaller = quickSort [a | a <- xs, a <= x]
      larger = quickSort [a | a <- xs, a > x]
  in smaller ++ [x] ++ larger


reducerange :: Int -> Int -> Int
reducerange start stop
    | start > stop = 0
    | otherwise = start + reducerange (start + 1) stop

l1 = [1,2,3,4]
l2 = 1 : 2 : 3 : []
 
sumAll :: [Int] -> Int
sumAll [] = 0
sumAll (x:xs) = x + sumAll xs
 
size :: [a] -> Int 
size [] = 0
size (_:xs) = 1 + size xs
 
flatten1 :: [[a]] -> [a]
flatten1 [] = []
flatten1 (x:xs) = x ++ flatten1 xs
 
flatten2 :: [[a]] -> [a]
flatten2 = foldr (++) []
