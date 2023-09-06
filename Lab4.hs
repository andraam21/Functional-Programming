import Data.Char
import Control.Applicative

data Nat = Zero | Succ Nat deriving Show
 
fromInt :: Int -> Maybe Nat 
fromInt x 
  | x < 0 = Nothing
  | otherwise = Just (get x)
      where get 0 = Zero
            get x = Succ (get (x-1))

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


data Expr = Atom Int | Var String | Plus Expr Expr  deriving Show

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
        aux (x:xs) = if (c == x) then [(x, xs)] else []

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser aux
    where
        aux "" = []
        aux (x:xs) = if (p x) then [(x, xs)] else []


-- failParser :: Parser a
-- failParser = Parser $ \s -> []
 
-- charParser :: Char -> Parser Char
-- charParser c = Parser $ \s -> case s of 
--                                 [] -> []
--                                 (x:xs) -> if x == c then [(c,xs)] else []
 
-- predicateParser :: (Char -> Bool) -> Parser Char
-- predicateParser p = Parser $
--   \s -> case s of 
--           [] -> []
--           (x:xs) -> if p x then [(x,xs)] else []



