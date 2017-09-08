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

startLogicLoop :: TVar L.Game -> TVar L.Players -> TVar M.Index -> IO()
startLogicLoop g p i = void $ L.logic p g i

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
  game <- newTVarIO L.gameDefault
  players <- newTVarIO L.playersDefault
  index <- newTVarIO (-1,-1)
  (window, (f, buttons)) <- G.setWindow index
  -- updater for Buttons
  -- TODO Maybe merge these two calls
  forkIO $ startLogicLoop game players index
  forkIO $ G.update game players buttons
  mainGUI




