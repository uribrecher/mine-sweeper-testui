{-# OPTIONS -Wall #-}

module MineSweeper
    ( Board
    , GameState
    , Action
    , parseBoard
    , parseAction
    , newGame
    , act
    , showGame
    , gameOver
    , isGameOn
    ) where

import System.Random
import System.Random.Shuffle
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Safe

data Board = Board
  { mines :: Set.Set (Int, Int)
  , width :: Int
  , height :: Int
  }

data CellState = Flagged | Dug Int deriving (Eq)
data Action = Dig Int Int | Flag Int Int deriving (Read)
data Status = Boom | Win | GameOn deriving (Eq)
newtype GameState = GameState {getState :: Map.Map (Int,Int) CellState}

parseNumber :: String -> Either String Int
parseNumber str =
  case readMay str of
    Nothing -> Left $ str ++ " is not a valid number"
    Just x -> Right x

parseArgs :: [String] -> Either String (Int,Int,Int)
parseArgs [widthStr, heightStr, mineStr] = do
  w <- parseNumber widthStr
  h <- parseNumber heightStr
  mineCount <- parseNumber mineStr
  return (w,h,mineCount)
parseArgs _ = Left "usage: minesweeper width height mineCount"

parseBoard :: (RandomGen g) => g -> [String] -> Either String Board
parseBoard gen args = do
  t <- parseArgs args
  parseRanges gen t

parseRanges :: (RandomGen g) => g -> (Int,Int,Int) -> Either String Board
parseRanges gen (w,h,mineCount)
  | w < 5 = Left "width is too small"
  | w > 20 = Left "width is too big"
  | h < 5 = Left "height is too small"
  | h > 20 = Left "height is too big"
  | mineCount < 4 = Left "mine count is too small"
  | mineCount > 199 = Left "mine count is too big"
  | mineCount >= w * h = Left "mine count is too big for board"
  | otherwise = Right (generateMines gen mineCount w h)

  
toTupple :: Action -> (Int,Int)
toTupple (Dig x y) = (x,y)
toTupple (Flag x y) = (x,y)

parseAction :: Board -> String -> Either String Action
parseAction b actionStr =
  let maybeAction = readMay actionStr
  in case maybeAction of
    Nothing -> Left "action has bad syntax"
    Just action -> isValidAction b action

isValidAction :: Board -> Action -> Either String Action
isValidAction Board {width=w,height=h} action
  | col < 1 = Left "column is too low"
  | col > w = Left "column is too high"
  | row < 1 = Left "row is too low"
  | row > h = Left "row is too high"
  | otherwise = Right action
  where (col,row) = toTupple action

showCellState :: CellState -> Char
showCellState Flagged = '!'
showCellState (Dug x) = chr (ord '0' + x)

showCell :: Char -> String
showCell c = ['[', c, ']']

showPad :: Int -> String
showPad x
  | x < 10 = "00" ++ show x
  | otherwise = '0' : show x

showRow :: Int -> Int -> Map.Map (Int,Int) Char -> String
showRow row w cells =
 unwords $
    showPad row : [ showCell $ fromMaybe ' ' $ Map.lookup (x,row) cells | x <- [1 .. w] ]

showCollumns :: Int -> String
showCollumns w =
   unwords $ "   " : map showPad [1..w]

showCells :: Int -> Int -> Map.Map (Int,Int) Char -> String
showCells w h cells =
 unlines $
  showCollumns w : [ showRow row w cells | row <- [1 .. h] ]

showGame :: Board -> GameState -> String
showGame Board {width=w,height=h} GameState {getState=state}  =
  showCells w h (fmap showCellState state)

generateMines :: RandomGen g => g -> Int -> Int -> Int -> Board
generateMines gen mineCount w h = Board m w h
  where
    pop = [(x, y) | x <- [1 .. w], y <- [1 .. h]]
    m = Set.fromList $ take mineCount $ shuffle' pop (length pop) gen

newGame :: GameState
newGame = GameState Map.empty

offsetList ::[(Int,Int)]
offsetList = [(0,1),(0,-1),(1,0),(-1,0),(1,1),(-1,-1),(1,-1),(-1,1)]

neighbors :: Int -> Int -> Set.Set (Int,Int)
neighbors x y = Set.fromList $ map (\(a,b)->(a+x,b+y)) offsetList

countAround :: Int -> Int -> Set.Set (Int,Int) -> Int
countAround x y mineSet =
  length $ Set.toList $ Set.intersection mineSet (neighbors x y)

isInsideBoard :: Int -> Int -> Board -> Bool
isInsideBoard x y Board {width=w, height=h} =
  x > 0 && y > 0 && x <= w && y <= h

digSingle :: Int -> Int -> Int -> GameState -> GameState
digSingle x y bombCount GameState {getState=oldState} =
  GameState $ Map.insert (x,y) (Dug bombCount) oldState

digAround :: Int -> Int -> Board -> GameState -> GameState
digAround x y b state = foldr (\(dx,dy) acc -> dig dx dy b acc) state (neighbors x y)

digAllowed :: Int -> Int -> Board -> GameState -> Bool
digAllowed x y b oldState = isInsideBoard x y b && not (Map.member (x, y) (getState oldState))

dig :: Int -> Int -> Board -> GameState -> GameState
dig x y b oldState
   | digAllowed x y b oldState = newState
   | otherwise = oldState
   where mineCount = countAround x y (mines b)
         singleState = digSingle x y mineCount oldState
         newState = if mineCount > 0
          then singleState
          else digAround x y b singleState
  
toggleMark :: Int -> Int -> GameState -> GameState
toggleMark x y GameState {getState=oldState} =
  case cellState of
    Nothing -> GameState $ Map.insert (x,y) Flagged oldState
    Just cell ->
       case cell of
        Dug _ -> GameState oldState
        Flagged -> GameState $ Map.delete (x,y) oldState
  where cellState = Map.lookup (x,y) oldState

isDug :: CellState -> Bool
isDug (Dug _) = True
isDug _ = False

gameStatus :: Board -> GameState -> Status
gameStatus Board {width=w, height=h, mines=m} GameState {getState=state} =
     let digs = Map.keysSet $ Map.filter isDug state
         explosions = Set.intersection digs m
     in if not $ Set.null explosions
         then Boom
         else if length digs + length m == w * h
           then Win
           else GameOn

isGameOn :: Board -> GameState -> Bool
isGameOn b state = gameStatus b state == GameOn

gameOver :: Board -> GameState -> String
gameOver b state
  | gameStatus b state == Boom = showExplosion b state
  | otherwise = showGame b state ++ "Winner!"

act :: Action -> Board -> GameState -> GameState
act (Dig x y) b oldState = dig x y b oldState
act (Flag x y) _ oldState = toggleMark x y oldState

createMinesMap :: Set.Set (Int,Int) -> Map.Map (Int,Int) Char
createMinesMap = foldr (\mine acc -> Map.insert mine '*' acc) Map.empty

showExplosion :: Board -> GameState -> String
showExplosion Board {mines=m,width=w,height=h} GameState {getState=state} =
  endBoard ++ "Boom! game is over"
  where cells = fmap showCellState state
        mineChars = createMinesMap m
        endBoard = showCells w h (Map.union mineChars cells)
