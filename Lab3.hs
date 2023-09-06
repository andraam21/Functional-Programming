import Data.List

data ChessResult = Win | Draw | Loss

instance Show ChessResult where
    show Win = "Win"
    show Draw = "Draw"
    show Loss = "Loss"


-- score :: [ChessResult] -> Float
-- score x = foldr (\result acc -> case result of
--                                     Win -> acc + 1
--                                     Loss -> acc
--                                     Draw -> acc + 0.5) 0 x

points :: ChessResult -> Float
points x = case x of
    Win -> 1
    Draw -> 0.5
    Loss -> 0

score :: [ChessResult] -> Float
score c = case c of 
    [] -> 0
    [x] -> points x
    (x:xs) -> (points x) + score xs

ranking :: [[ChessResult]] -> [Float]
ranking r = sortBy compare (map score r)

earnings :: [[ChessResult]] -> Float -> [Float]
earnings playerScores prizeMoney = map (\f -> f * prizeMoney / (foldl (+) 0 (ranking playerScores))) (ranking playerScores)


data Player = Player {name :: String, elo :: Float, tournamentGames :: [ChessResult]}
data Tree a = Null | Node a (Tree a) (Tree a) deriving Show
type Tournament = Tree Player

instance Eq Player where
  (==) p1 p2 = score (tournamentGames p1) == score (tournamentGames p2)

-- instance Eq Player where
--   Player n1 e1 t1 == Player n2 e2 t2 = score t1 == score t2

instance Ord Player where
  compare p1 p2 = compare (score (tournamentGames p1)) (score (tournamentGames p2))

addResult :: ChessResult -> Player -> Player
addResult cr (Player n e t) = Player n e (cr : t)

winningOdds :: Float -> Float -> Float
winningOdds eloA eloB = 1 / (1 + 10** ((eloB-eloA)/400))
 

playGame :: Player -> Player -> (Player, Player)
playGame player1 player2
  | odds > 0.5 = (addResult Win player1, addResult Loss player2)
  | odds < 0.5 = (addResult Loss player1, addResult Win player2)
  | otherwise = (addResult Draw player1, addResult Draw player2)
  where
    odds = winningOdds (elo player1) (elo player2)

multipleMatches :: Player -> [Player] -> (Player, [Player])
multipleMatches player opponents = mapAccumL (\p opp -> playGame p opp) player opponents

groupStage :: [Player] -> [Player]
groupStage players = concatMap (\p -> let (updatedPlayer, _) = multipleMatches p (filter (/= p) players) in updatedPlayer) players

tournamentStage :: [Player] -> Tournament
tournamentStage [] = Null
tournamentStage [p] = Node p Null Null
tournamentStage players = tournamentStage (groupStage players)

chessTournament :: [Player] -> Tournament
chessTournament players = tournamentStage (groupStage players)

findPlayerScoreByName :: String -> Tournament -> Maybe Float
findPlayerScoreByName _ Null = Nothing
findPlayerScoreByName name_find (Node player left right)
  | name_find == name player = Just (score (tournamentGames player))
  | name_find < name player = findPlayerScoreByName name_find left
  | otherwise = findPlayerScoreByName name_find right
