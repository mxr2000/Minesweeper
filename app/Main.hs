module Main (main) where

import Control.Monad (guard)
import qualified Data.Set as S
import qualified GHC.IO.Handle as IO
import System.IO
import qualified System.Random as R
import qualified Text.Printf as FMT
import Text.Read (readMaybe)

data Cell
  = Mine
  | Plain
  | Clue Int
  deriving (Show)

type Grid = [[Cell]]

type Pos = (Int, Int)

data State = State
  { grid :: Grid,
    flagPos :: S.Set Pos,
    exploredPos :: S.Set Pos
  }

data Operation
  = Open Pos
  | Flag Pos

-- check if a pos is in bounds of a grid
isInBound :: Grid -> Pos -> Bool
isInBound grid (x, y) = x >= 0 && x < length grid && y >= 0 && y < length (head grid)

-- flag a cell if a pos is not flagged nor explored
flagCell :: State -> Pos -> State
flagCell state@(State _ flagged explored) pos
  | S.member pos flagged = state {flagPos = S.delete pos flagged}
  | S.member pos explored = state
  | otherwise = state {flagPos = S.insert pos flagged}

-- open cells with dfs
openCells :: State -> Pos -> State
openCells state@(State grid flagged explored) pos@(x, y)
  | S.member pos explored = state
  | S.member pos flagged = state
  | otherwise = case grid !! x !! y of
    Mine -> state
    _ -> foldl openCells (state {exploredPos = S.insert pos explored}) (surroundingPos height width pos)
  where
    height = length grid
    width = length $ head grid

-- open a cell and return nothing if a mine is opened
openMaybe :: State -> Pos -> Maybe State
openMaybe s p@(x, y)
  | isInBound (grid s) p = case grid s !! x !! y of
    Mine -> Nothing
    Plain -> Just (openCells s p)
    Clue _ -> Just (openCells s p)
  | otherwise = Just s

-- map a pos index to a pos
numToPos :: Int -> Int -> Pos
numToPos width num = (num `div` width, num `mod` width)

-- get all legal surrounding positions around a pos
surroundingPos :: Int -> Int -> Pos -> S.Set Pos
surroundingPos height width (x, y) =
  S.fromList $
    filter
      isValid
      [ (x - 1, y - 1),
        (x - 1, y),
        (x - 1, y + 1),
        (x, y - 1),
        (x, y + 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1)
      ]
  where
    isValid :: Pos -> Bool
    isValid (i, j) = i >= 0 && i < height && j >= 0 && j < width

-- check if a pos is a mine
isMine :: Grid -> Pos -> Bool
isMine grid (x, y) = case grid !! x !! y of
  Mine -> True
  _ -> False

-- make clue for every plain that is adjacent to mines
makeClue :: Grid -> Grid
makeClue grid = do
  i <- [0 .. (height - 1)]
  return $ do
    j <- [0 .. (width - 1)]
    let cell = grid !! i !! j
    let cntSurroundingMines = countSurrounding (i, j)
    return $ case cell of
      Mine -> Mine
      Plain -> if cntSurroundingMines == 0 then Plain else Clue cntSurroundingMines
      _ -> Plain
  where
    height = length grid
    width = length $ head grid

    countSurrounding :: Pos -> Int
    countSurrounding pos = length $ S.filter (isMine grid) (surroundingPos height width pos)

-- generate s random pos not in the visited set
generateRandomPos :: R.StdGen -> Int -> Int -> S.Set Int -> (Pos, R.StdGen, S.Set Int)
generateRandomPos g height width visited =
  let result = R.randomR (0, height * width - 1) g
   in case result of
        (num, newG) ->
          if S.member num visited
            then generateRandomPos newG height width visited
            else (numToPos width num, newG, S.insert num visited)

-- generate a set of positions with the grid size
generateNRandomPos :: R.StdGen -> S.Set Int -> Int -> Int -> Int -> S.Set Pos
generateNRandomPos g visited height width cnt
  | cnt == 0 = S.empty
  | otherwise = S.insert pos $ generateNRandomPos newG newVisited height width (cnt - 1)
  where
    (pos, newG, newVisited) = generateRandomPos g height width visited

-- generate a grid by size and number of mines
generateGrid :: Int -> Int -> Int -> IO Grid
generateGrid height width cnt = do
  g <- R.newStdGen
  let positions = generateNRandomPos g S.empty height width cnt
  let raw = do
        i <- [0 .. (height - 1)]
        return [if S.member (i, j) positions then Mine else Plain | j <- [0 .. (width - 1)]]
  let result = makeClue raw
  return result

-- print a single cell
printCell :: State -> Pos -> Bool -> String
printCell (State g flagged explored) pos@(x, y) showMines =
  if not showMines && S.member pos flagged
    then "F"
    else case cell of
      Plain -> if S.member pos explored then "." else "#"
      Mine -> if showMines then "*" else "#"
      Clue n -> if S.member pos explored then show n else "#"
  where
    cell = g !! x !! y

formatHeadLevel :: Int -> Int -> String
formatHeadLevel range strWidth =
  "   " ++ concat [formatDigit i | i <- [0 .. range]]
  where
    formatDigit :: Int -> String
    formatDigit num = FMT.printf ("%-" ++ show strWidth ++ "d") digit
      where
        digit = num `mod` 10
        
formatHead :: Int -> Int -> String
formatHead range strWidth
  | range == 0 = ""
  | otherwise = formatHead (range `div` 10) (strWidth * 10) ++ "\n" ++ cur
  where 
    cur = formatHeadLevel range strWidth
       

printHead :: State -> IO ()
printHead (State g _ _) =
  putStrLn $ formatHead (length (head g) - 1) 2

-- print the grid
printGrid :: State -> IO ()
printGrid state@(State g _ _) = do
  printHead state
  putStrLn $ unlines [FMT.printf "%-2d " x ++ unwords [printCell state (x, y) False | y <- [0 .. (length (head g) - 1)]] | x <- [0 .. (length g - 1)]]

-- read from command and parse to an operation
readInput :: Grid -> IO (Maybe Operation)
readInput grid = do
  putStr "(o: open | f : flag) (x) (y): "
  line <- getLine
  let parts = words line
  return $ case parts of
    [p1, p2, p3] -> do
      x <- readMaybe p2
      y <- readMaybe p3
      let pos = (x, y)
      guard (isInBound grid pos)
      case p1 of
        "o" -> Just $ Open pos
        "f" -> Just $ Flag pos
        _ -> Nothing
    _ -> Nothing

-- judge if game is won by checking the set of positions of mines is a subset of flagged position set
hasWon :: State -> Bool
hasWon (State grid flagged _) =
  S.isSubsetOf (S.fromList minePos) flagged
  where
    height = length grid
    width = length $ head grid
    minePos :: [Pos]
    minePos = do
      i <- [0 .. (height - 1)]
      j <- [0 .. (width - 1)]
      let pos = (i, j)
      guard (isMine grid pos)
      return pos

runGame :: State -> IO ()
runGame s = do
  printGrid s
  input <- readInput (grid s)
  case input of
    Just (Open pos) -> do
      case openMaybe s pos of
        Nothing -> do
          putStrLn "Game over"
        Just ns -> runGame ns
    Just (Flag pos) -> do
      let ns = flagCell s pos
      if hasWon ns
        then putStrLn "You Won"
        else runGame ns
    Nothing -> do
      putStrLn "Wrong format"
      runGame s

main :: IO ()
main = do
  IO.hSetBuffering stdout IO.NoBuffering
  let height = 4
  let width = 12
  let mineCnt = 1
  grid <- generateGrid height width mineCnt
  let state =
        State
          { grid = grid,
            flagPos = S.empty,
            exploredPos = S.empty
            --exploredPos = S.fromList [(i, j) | i <- [0 .. (height - 1)], j <- [0 .. (width - 1)]]
          }
  runGame state
