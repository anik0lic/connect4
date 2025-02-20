import Data.List
import Rose

roseExample :: Rose Integer
roseExample = generateRose 2 (\x -> [x+1, x+2, x+3]) 2

data Player = Red | Yellow deriving (Show, Eq)

data Cell = Empty | Occupied Player deriving (Show, Eq)
newtype Column a = Column [a] deriving (Show, Eq)
newtype Board a = Board [Column a] deriving (Show)

data BoardState a = BoardState { board :: Board a, currentPlayer :: Player } deriving (Show)

emptyBoard :: Int -> Int -> Board Cell
emptyBoard rows cols = Board $ replicate cols (Column $ replicate rows Empty)

cellToStr :: Cell -> String
cellToStr Empty = "."
cellToStr (Occupied Red) = "C"
cellToStr (Occupied Yellow) = "Z"

rowToStr :: [Cell] -> String
rowToStr row = "|" ++ concat [cellToStr cell ++ "|" | cell <- row]

boardToStr :: Board Cell -> String
boardToStr (Board cols) = unlines (map rowToStr rows)
  where
    rows = transpose $ map (\(Column col) -> col) cols

printBoard :: Board Cell -> IO()
printBoard board = putStrLn $ boardToStr board

boardExmpl = Board [Column [Empty, Occupied Red, Occupied Red, Occupied Yellow, Occupied Red], 
                      Column [Empty, Empty, Occupied Yellow, Occupied Yellow, Occupied Yellow], 
                      Column [Empty, Empty, Occupied Yellow, Occupied Yellow, Occupied Red],
                      Column [Empty, Empty, Occupied Yellow, Occupied Yellow, Occupied Yellow],
                      Column [Empty, Occupied Red, Occupied Red, Occupied Red, Occupied Yellow]]

fullBoard = Board [Column [Occupied Yellow, Occupied Red, Occupied Red, Occupied Red, Occupied Yellow], 
                   Column [Occupied Yellow, Occupied Red, Occupied Yellow, Occupied Red, Occupied Red], 
                   Column [Occupied Red, Occupied Yellow, Occupied Yellow, Occupied Yellow, Occupied Red],
                   Column [Occupied Yellow, Occupied Red, Occupied Yellow, Occupied Red, Occupied Yellow],
                   Column [Occupied Yellow, Occupied Yellow, Occupied Red, Occupied Red, Occupied Red]]

validMoves :: Board Cell -> [Int]
validMoves (Board columns) = [i | (i, Column col) <- zip [0..] columns, Empty `elem` col]

boardStateExample = BoardState boardExmpl Yellow
fullBoardState = BoardState fullBoard Red

getLeft :: Either a b -> a
getLeft (Left l) = l

getRight :: Either a b -> b
getRight (Right r) = r

replaceFirstEmpty :: Player -> [Cell] -> [Cell]
replaceFirstEmpty player cells = reverse (replaceFirstEmptyFromEnd (reverse cells))
  where
    replaceFirstEmptyFromEnd (Empty : rest) = Occupied player : rest
    replaceFirstEmptyFromEnd (cell : rest) = cell : replaceFirstEmptyFromEnd rest
    replaceFirstEmptyFromEnd [] = []

makeMove :: Int -> BoardState Cell -> Either (BoardState Cell, String) (BoardState Cell)
makeMove col (BoardState (Board columns) currentPlayer)
    | col `elem` validMoves (Board columns) = 
        let Column cells = columns !! col
            newColumns = take col columns ++ [Column (replaceFirstEmpty currentPlayer cells)] ++ drop (col + 1) columns
            newBoard = Board newColumns
            newPlayer = nextPlayer currentPlayer
            newState = BoardState newBoard newPlayer
        in if checkWin currentPlayer newBoard
           then Left (BoardState newBoard currentPlayer, show currentPlayer ++ " wins")
           else if isDraw newBoard
                then Left (BoardState newBoard currentPlayer, "It's a draw")
                else Right newState
    | otherwise = Left (BoardState (Board columns) currentPlayer, "Invalid move")
  where
    nextPlayer :: Player -> Player
    nextPlayer Red = Yellow
    nextPlayer Yellow = Red

winningCombinations :: Board Cell -> [[Cell]]
winningCombinations (Board columns) = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  where
    rows = transpose $ map (\(Column col) -> col) columns
    horizontal = rows
    vertical = map (\(Column col) -> col) columns
    diagonal1 = diagonals rows
    diagonal2 = diagonals (map reverse rows)

    diagonals :: [[a]] -> [[a]]
    diagonals xss = [ [ xss !! (i + k) !! k | k <- [0..min (length xss - i - 1) (length (head xss) - 1)] ] | i <- [0..length xss - 1] ]
                 ++ [ [ xss !! k !! (i + k) | k <- [0..min (length xss - 1) (length (head xss) - i - 1)] ] | i <- [1..length (head xss) - 1] ]

-- 1 . . .
-- 1 2 . .
-- 1 2 3 .
-- 1 2 3 4

checkWin :: Player -> Board Cell -> Bool
checkWin player board = any (hasFourInARow . map cellToPlayer) (winningCombinations board)
  where
    cellToPlayer :: Cell -> Maybe Player
    cellToPlayer (Occupied p) | p == player = Just player
    cellToPlayer _ = Nothing

    hasFourInARow :: [Maybe Player] -> Bool
    hasFourInARow = any ((>= 4) . length) . filter (all (== Just player)) . group

isDraw :: Board Cell -> Bool
isDraw (Board columns) = all (notElem Empty . (\(Column col) -> col)) columns

genBoard = Board [Column [Empty, Occupied Yellow, Occupied Yellow, Occupied Red, Occupied Red], 
                         Column [Empty, Empty, Occupied Red, Occupied Yellow, Occupied Yellow], 
                         Column [Empty, Empty, Empty, Occupied Yellow, Occupied Red],
                         Column [Empty, Occupied Red, Occupied Red, Occupied Red, Occupied Yellow],
                         Column [Empty, Occupied Red, Occupied Yellow, Occupied Yellow, Occupied Red]]

genExample = BoardState genBoard Red

nextBoardStates :: BoardState Cell -> [BoardState Cell]
nextBoardStates state = 
  let BoardState board currentPlayer = state
  in [newState | col <- validMoves board, 
                let result = makeMove col state, 
                Right newState <- [result]]

generateGameTree :: Int -> BoardState Cell -> Rose (BoardState Cell)
generateGameTree depth = generateRose depth nextBoardStates

newtype GameStateError = Invalid String deriving (Show, Eq)
newtype GameStateOp b a = GameStateOp { runGameStateOp :: BoardState b -> Either GameStateError (a, BoardState b) }

instance Functor (GameStateOp b) where
  fmap f (GameStateOp g) = GameStateOp $ \s -> case g s of
    Left err -> Left err
    Right (a, s') -> Right (f a, s')

instance Applicative (GameStateOp b) where
    pure x = GameStateOp $ \s -> Right (x, s)
    (GameStateOp f) <*> (GameStateOp g) = GameStateOp $ \s -> case f s of
        Left err -> Left err
        Right (a, s') -> case g s' of
            Left err -> Left err
            Right (b, s'') -> Right (a b, s'')

instance Monad (GameStateOp b) where
    (GameStateOp h) >>= f = GameStateOp $ \s -> case h s of
        Left err -> Left err
        Right (a, s') -> let (GameStateOp g) = f a in g s'