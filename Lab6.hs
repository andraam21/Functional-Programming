-- Last year labs --

myand :: Bool -> Bool -> Bool
myand True True = True
myand _ _ = False

len :: [Int] -> Int
len [] = 0
len [x] = 1
len (x : xs) = 1 + len xs

largest :: Int -> Int -> Int -> Int
largest a b c = if (a > b && a > c) then a else if (b > a && b > c) then b else c

booltoint :: [Bool] -> [Int]
booltoint lst = foldr (\bl acc -> if bl == True then 1 : acc else 0 : acc) [] lst

sumofboolean :: [Bool] -> Int
sumofboolean lst = foldr (\bl acc -> if bl == True then acc + 1 else acc) 0 lst

isthirdodd :: [Int] -> Bool
isthirdodd lst = if ((mod (lst !! 3) 2) == 1) then True else False

inssort :: [Integer] -> [Integer]
inssort [] = []
inssort [a] = [a]
inssort (x:xs) = insertaux x (inssort xs)
    where
        insertaux el [a] = if el < a then [el, a] else [a, el]
        insertaux el (y:ys) = if el < y then el : y : ys else y : insertaux el ys


flatten :: [[Int]] -> [Int]
flatten [] = []
flatten [a] = a
flatten (x:xs) = x ++ flatten xs
-- flatten lst = foldr (\e acc -> acc ++ e) [] lst

type Matrix = [[Int]]  

prod :: Int -> Matrix -> Matrix
prod v mat = map (\row -> map (\el -> v * el) row) mat

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose b = (map head b):(transpose (map tail b))

vertical :: Matrix -> Matrix -> Matrix
vertical mat1 mat2 = mat1 ++ mat2

horiz :: Matrix -> Matrix -> Matrix
horiz mat1 mat2 = transpose ((transpose mat1) ++ (transpose mat2))

cut :: Matrix -> Matrix -> Matrix
cut mask image = map (\row -> map (\(elem1, elem2) -> if elem1 == 1 then elem2 else 0) row ) (zipWith (zipWith (,)) mask image)

addmat :: Matrix -> Matrix -> Matrix
addmat mat1 mat2 = zipWith (zipWith (+)) mat1 mat2

placeZerosAboveDiagonal :: Num a => [[a]] -> [[a]]
placeZerosAboveDiagonal matrix = zipWith (\row idx -> take idx row ++ replicate (length row - idx) 0) matrix [0..]

-- For the logo and a mask
intr :: [[Char]] -> [[Char]] -> [[Char]]
intr lg mk = map (\row -> map (\(el1, el2) -> if el1 == el2 then '*' else ' ') row) (zipWith (zipWith (,)) lg mk) -- char '' not "" string !!

getgrup :: String -> [(String, [String])] -> [String]
getgrup grp lst = foldr (\(group, names) acc -> if grp == group then (filter (\name -> head name == 'M') names) ++ acc else acc) [] lst
-- getgrup grp lst = head (map (\(grp, name) -> name) (filter (\(grup, name) -> grup == grp) lst))

longerthan :: Int -> [String] -> [String]
longerthan sz lst = foldr (\name acc-> if length name > sz then name : acc else acc) [] lst
-- longerthan sz lst = filter (\email -> length email > sz) lst

bigname :: Int -> [String] -> [Bool]
bigname sz lst = foldr (\el acc -> if length el > sz then True : acc else False : acc) [] (map (\name -> words name) lst)

splitString :: String -> [String]
splitString str = words str -- split a phrase into a list of words

mymap :: (Int -> Int) -> [Int] -> [Int]
mymap f lst = foldr (\el acc -> f el : acc) [] lst

intersection :: (Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)
intersection s1 s2 = \x -> s1 x && s2 x


-- Others --




data Nat = Zero | Succ Nat deriving (Show)

instance Eq Nat where
    Zero == Zero = True
    Succ a == Succ b = a == b
    _ == _ = False

tonat :: Int -> Nat 
tonat 0 = Zero
tonat x = Succ (tonat (x - 1))

plus :: Nat -> Nat -> Nat 
plus Zero x = x 
plus (Succ a) b = Succ (plus a b)

mul :: Nat -> Nat -> Nat 
mul Zero _ = Zero
mul (Succ a) b =  plus a (mul a b)


data List a = Void | Cons a (List a)

mylenlista :: List a -> Int
mylenlista Void = 0
mylenlista (Cons x ls) = 1 + mylenlista ls

cast :: List a -> [a]
cast Void = []
cast (Cons x ls) = x : (cast ls)


-- data List a = Null | Cons a (List a)
-- data BTree a = Void | Node a (BTree a) (BTree a)

-- data Student = Student {
--   first_name :: String,
--   last_name  :: String,
--   grades     :: [Float]
-- } -- in order to have names allied to every field

data Student = Student String String [Float] deriving (Show)

instance Eq Student where 
    Student n1 s1 m1 == Student n2 s2 m2 = avg m1 == avg m2
    -- _ == _ = False 

instance Ord Student where 
    compare (Student n1 s1 m1) (Student n2 s2 m2) = compare (avg m1) (avg m2)

avg :: [Float] -> Float
avg lst = (foldr (+) 0 lst) / (fromIntegral (length lst))


-- We can crete a class to override some methods for datas
class IsVoid a where
  isVoid :: a -> Bool
 
-- instance IsVoid Bool where
--   isVoid False = True
--   isVoid True  = False
 
-- instance IsVoid (BTree a) where
--   isVoid Void = True
--   isVoid _    = False
 
instance IsVoid [a] where
  isVoid [] = True
  isVoid _  = False

-- That s how a functor looks like
class Functor f where
    fmap :: (a -> b) -> f a -> f b

data BTree a = EmptyTree | Node a (BTree a) (BTree a)

-- Enroll Btree in Eq
instance Eq a => Eq (BTree a) where
    EmptyTree == EmptyTree = True
    Node val1 l1 r1 == Node val2 l2 r2 = (val1 == val2) && (l1 == l2) && (r1 == r2)
    _ == _ = False


