module Main where

import Control.Concurrent
import Control.Monad (void, forever)
import System.Exit
import Control.Concurrent.STM.TVar
import Graphics.UI.Gtk

import qualified Logic as L
import qualified Material as M
import qualified Recommender as R
import qualified GUI as G


{-
  Game without GUI
  -- Needs a change in the possibleFunctions
-}
{-
main = do
  game <- newTVarIO L.gameDefault
  players <- newTVarIO L.playersDefault
  index <- newTVarIO (-1,-1)
  forkIO $ startLogicLoop game players index
  threadDelay 1000000000
  --exitSuccess


-}

{-
  For testing purposes. No GUI, no loop.
  Change ghc-options in .cabal file to: -threaded -rtsopts
  Run executable with: +RTS -Nx -s
    where x i the number of cores
-}
{-
main = do
  --let m = L.check5 M.emptyBoardL
  --print m
  r <- R.recom [] [M.Player 1, M.Player 2] M.emptyBoard
  print r
  return ()


-}
{-
  Game with GUI
-}

main = do
  initGUI
  game <- newTVarIO L.gameDefault
  players <- newTVarIO L.playersDefault
  index <- newTVarIO (1,1)
  let state = L.StateVars players game index
  (window, (f, buttons), rs) <- G.setWindow state
  -- runs the game logic
  forkIO $ startLogic3Loop state
  -- updates the GUI
  forkIO $ G.update state buttons rs
  mainGUI

{-
-- Starts the loop
startLogicLoop :: L.StateVars -> IO()
startLogicLoop s = void $ L.logic2 (L.tplayers s) (L.tgame s) (L.tindex s)
-}

startLogic3Loop s = void $ L.logic3 s