-- Function declare

f1 = \x y -> x + y
f2 x y = x + y

-- Sugar syntax

g1 a b = (+) a b
g2 a b = a + b 

-- Call the functions : g1 5 6 
--                      5 `g1` 6


-- Lists

lst1 :: [Int]
lst1 = 1:2:3:[]

lst2 :: [Int]
lst2 = [1, 2, 3]

lst3 :: [Int]
lst3 = [1, 3 .. 10] -- hop of 2

-- head lst3  (returns the first elem)
-- last lst3  (returns the last elem)
-- tail lst3  (returns everything but the first elem)
-- init

-- concatenate : lst1 ++ lst2

-- Pairs

pair = (3, "Andra")
-- fst pair
-- snd pair

-- If i have data Player = {name :: String}    -> name p1  (where Player p1)

--  Functions

factorial_pm 1 = 1
factorial_pm x = x * factorial_pm (x - 1)

factorial_if x = if x == 1 then 1 else x * factorial_if (x - 1)

factorial_guards x
    | x == 1 = 1
    | otherwise = x * factorial_guards (x - 1)

factorial_case x = case x of
                        1 -> 1
                        _ -> x * factorial_case (x - 1)


length_pm [] = 0
length_pm (x:xs) = 1 + length_pm xs

length_if l = if l == [] then 0 else 1 + length_if (tail l)

length_guards l
    | l == [] = 0
    | otherwise = 1 + length_guards (tail l)

length_case l = case l of 
    [] -> 0
    (x:xs) -> 1 + length_case xs


-- Curry vs Uncurry

adduncurry :: Int -> Int -> Int
adduncurry a b = a + b

addcurry :: (Int, Int) -> Int
addcurry (a, b) = a + b


dup :: [Int] -> [Int]
dup [] = []
dup l@(x:xs) = x:l -- eq to dup(x:xs) = x:(x:xs)

mymap l = map (\x -> 2*x+1) l
mymap1 l = map ((+1) . (*2)) l

suma :: [Int] -> Int
suma l = sum_aux 0 l
    where sum_aux acc lst = if lst == [] then acc else sum_aux (acc + (head lst)) (tail lst)

-- Usually 3 cases for a list
sum1 :: [Int] -> Int
sum1 [] = 0
sum1 [x] = x
sum1 (x:xs) = x + sum1 xs


fizzbuzz :: [Int] -> [String]
fizzbuzz lst = map fizzbuzzhelper lst
    where 
        fizzbuzzhelper :: Int -> String
        fizzbuzzhelper x = if mod x 15 == 0 then "Fizzbuzz"
                            else if mod x 5 == 0 then "fizz"
                            else if mod x 3 == 0 then "buzz"
                            else show x

reduce :: Int -> Int -> Int
reduce start stop = redaux 0 start stop
    where
        redaux :: Int-> Int-> Int-> Int
        redaux acc beg end 
            | beg > end = acc
            | otherwise = redaux (acc + beg) (beg + 1) end

data Nat = Zero | Succ Nat deriving Show

fromInt :: Int -> Maybe Nat
fromInt int 
    | int < 0 = Nothing
    | int == 0 = Zero
    | otherwise = Just(get int)
        where get a = Succ (get (a - 1))

-- Recursive solution
sol0 :: [Int] -> Int
sol0 prices = max_profit 0 ((length prices) - 1) 1
	where max_profit left right year 
			| left > right = 0
			| otherwise = maximum [year * (prices !! left) + (max_profit (left+1) right (year+1)),
								   year * (prices !! right) + (max_profit left (right-1) (year+1)) ]

-- For exam --

-- Enroll in Eq 

data BTree = Node Int BTree BTree | Nil deriving Show
instance Eq BTree where
    Nil == Nil = True
    Node val1 left1 right1 == Node val2 left2 right2 = val1 == val2 && left1 == left2 && right1 == right2
    _ == _ = False

data Person = Person {name :: String, cnp :: Integer}
instance Eq Person where
    Person name1 cnp1 == Person name2 cnp2 = name1 == name2 && cnp1 == cnp2
    p1 /= p2 = not (p1 == p2)  -- _ == _ = False


data BST a = Empty | Node a (BST a) (BST a)
instance Eq a => Eq (BST a) where
    Empty == Empty = True
    Node info1 l1 r1 == Node info2 l2 r2 = info1 == info2 && l1 == l2 && r1 == r2
    _ == _ = False

data Player = Player {name :: String, elo :: Float, tournamentGames :: [ChessResult]} -- Player Int String (without names i can t acces the fields)
instance Eq Player where
  (==) p1 p2 = score (tournamentGames p1) == score (tournamentGames p2)
-- OR --
instance Eq Player where
  Player n1 e1 t1 == Player n2 e2 t2 = score t1 == score t2

data Option a = None | Some a deriving Show
instance Eq a => Eq (Option a) where
    None == None = True
    Some a == Some b = a == b
    _ == _ = False

data Option a = None | Some a deriving Show

instance Ord a => Ord (Option a) where
    compare None None = EQ
    compare None Some _ = LT
    compare Some _ None = GT
    compare Some x Some y = compare x y


instance Ord Player where
  compare p1 p2 = compare (score (tournamentGames p1)) (score (tournamentGames p2))
--  OR --
instance Ord Player where
  compare (Player n1 e1 t1) (Player n2 e2 t2) = compare (score t1) (score t2)


instance Show Nat where
  show = Prelude.show . toInt
 
instance Show Expr where
  show (Atom x) = Prelude.show x
  show (Plus e1 e2) = (show e1) ++ " + " ++ (show e2)
  show (Mult e1 e2) = (show e1) ++ " * " ++ (show e2)

-- Functors --

data NestedList a = Val a | Nest [NestedList a] 
instance Functor NestedList where
    fmap f (Val x) = Val (f x)
    fmap f (Nest nestx) = Nest (map (fmap f) nestx)

instance Functor List where
  fmap f Void = Void
  fmap f (Cons x xs) = Cons (f x) (map f xs)
 
instance Functor Option where 
  fmap f None = None
  fmap f (Some x) = Some (f x)

-- Monads --

minus :: Nat -> Nat -> Maybe Nat
minus Zero Zero = Just Zero
minus (Succ a) (Succ b) = minus a b
minus y Zero = Just y
minus _ _ = Nothing

mminus :: Maybe Nat -> Maybe Nat -> Maybe Nat
mminus m n = 
    do 
        nat1 <- m
        nat2 <- n
        minus nat1 nat2

plus :: Nat -> Nat -> Nat
plus Zero a = a
plus (Succ a) b = Succ (plus a b)

mplus :: Maybe Nat -> Maybe Nat -> Maybe Nat
mplus m n =
    do
        nat1 <- m
        nat2 <- n 
        return (plus nat1 nat2)

multi :: Nat -> Nat -> Nat
multi Zero _ = Zero
multi a (Succ b) = plus a (multi a b)

mmulti :: Maybe Nat -> Maybe Nat -> Maybe Nat
mmulti m n = 
    do
        nat1 <- m
        nat2 <- n
        return (multi nat1 nat2)

-- Parsers --

data Parser a = Parser (String -> [(a,String)])
parse (Parser p) s = p s

failParser :: a -> Parser a 
failParser a = Parser aux
    where 
        aux _ = []

charParser :: Char -> Parser Char
charParser c = Parser aux
    where 
        aux "" = []
        aux (x:xs) = if c == x then [(x, xs)] else []

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser aux
    where
        aux "" = []
        aux (x:xs) = if (p x) then [(x, xs)] else []




