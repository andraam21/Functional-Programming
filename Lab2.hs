nats :: [Int]
nats = [0..]

odds :: [Int]
odds = [x | x <- nats, x `mod` 2 == 1]

even :: [Int]
even = [x | x <- nats, x `mod` 2 == 0]

squares :: [Int]
squares = [x * x | x <- nats]

fibs :: [Double]
fibs = [0, 1] ++ (zipWith (+) fibs (tail fibs))


data BTree = Node Int BTree BTree | Nil deriving Show

-- !!
-- instance Eq BTree where
--     Nil == Nil = True
--     Node val1 left1 right1 == Node val2 left2 right2 = val1 == val2 && left1 == left2 && right1 == right2
--     _ == _ = False
-- !!

data StreamBTree = StreamNode Int StreamBTree StreamBTree deriving Show
  
tree :: BTree
tree = Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)

repeatTree :: Int -> StreamBTree
repeatTree k = StreamNode k (repeatTree k) (repeatTree k)

sliceTree :: Int -> StreamBTree -> BTree
sliceTree 0 _ = Nil
sliceTree k (StreamNode x l r) = Node x (sliceTree (k - 1) l) (sliceTree (k - 1) r)  

generateTree :: Int -> (Int -> Int) -> (Int -> Int) -> StreamBTree
generateTree root l r = StreamNode root (generateTree (l root) l r) (generateTree (r root) l r)


build :: (Double -> Double) -> Double -> [Double]
build op a0 = a0 : build op (op a0)

op1 :: Double -> Double
op1 x
    | x == 0 = 1
    | x == 1 = 0
alternatingBinary :: [Double]
alternatingBinary = build op1 0
 
op2 :: Double -> Double
op2 x = ((abs x) + 1) * ((-1)**(x + 1))
alternatingCons :: [Double]
alternatingCons = build op2 0
 
select :: Double -> [Double] -> Double
select e (x:y:ys)
    | abs (x - y) < e = y
    | otherwise = select e (y:ys)

phiApprox :: Double
phiApprox = select 0.00001 [fn1 / fn | (fn, fn1) <- zip fibs (tail fibs)]

piApprox :: Double
piApprox = select 0.00001 (build (\an -> an + sin an) 1.0)

sqrtApprox :: Double -> Double
sqrtApprox k = select 0.00001 (build (\an -> 0.5 * (an + k / an)) 1.0)

derivativeApprox :: (Double -> Double) -> Double -> Double
derivativeApprox f a = select 0.00001 (build (\h -> (f (a + h) - f a) / h) 1.0)

