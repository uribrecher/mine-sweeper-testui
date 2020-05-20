{-# OPTIONS -Wall #-}

module Main where

import System.Environment
import System.Random
import MineSweeper

main :: IO ()
main = do
  args <- getArgs
  gen <- getStdGen
  let result = parseBoard gen args
      gameState = newGame
  case result of
      Left err -> putStrLn err
      Right board -> do
        finalState <- gameStep board gameState
        putStrLn $ gameOver board finalState

gameStep :: Board -> GameState -> IO GameState
gameStep board oldState = do
   putStrLn $ showGame board oldState
   action <- readAction board
   let newState = act action board oldState
   if isGameOn board newState
      then gameStep board newState
      else return newState

readAction :: Board -> IO Action
readAction board = do
  putStrLn "what is your next move?"
  actionStr <- getLine
  let result = parseAction board actionStr
  case result of
    Left msg -> do
      putStrLn msg
      readAction board
    Right action -> return action

