module Main where

import Control.Concurrent
import Control.Monad (void, forever)
import System.Exit
import Control.Concurrent.STM.TVar
import Graphics.UI.Gtk

import qualified Logic as L
import qualified Material as M
import qualified Recommender as R


{-
  Game without GUI
-}
{-
main = do
  game <- newTVarIO L.gameDefault
  players <- newTVarIO L.playersDefault
  forkIO $ startLogicLoop game players
  threadDelay 1000000000
  --exitSuccess
-}

startLogicLoop :: TVar L.Game -> TVar L.Players -> IO()
startLogicLoop g p = void $ L.logic (return p) (return g)

{- -- Moved to startLogicLoop
startLoop :: IO ()
startLoop = void (L.loop $ return (M.emptyBoard, L.playersStandard))
-}

{-
  Function for testing parallelism of R.recom and L.check5
-}
{-
main = do
  let m = L.check5 M.emptyBoardL
  r <- R.recom [] [M.Player 1, M.Player 2] M.emptyBoardL
  --print m
  print r
  return ()
-}

{-
  Game with GUI
-}

main = do
  initGUI
  window <- windowNew
  game <- newTVarIO L.gameDefault
  players <- newTVarIO L.playersDefault



  return ()

createBoard :: TVar game -> TVar players -> IO HBox
createBoard


















