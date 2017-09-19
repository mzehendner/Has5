module Main
  (
    main
  )
  where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Graphics.UI.Gtk

import qualified Logic as L
import qualified GUI as G

main :: IO ()
main = do
  _ <- initGUI
  game <- newTVarIO L.gameDefault
  players <- newTVarIO L.playersDefault
  index <- newTVarIO (1,1)
  let state = L.StateVars players game index
  (_window, (_f, buttons), rs) <- G.setWindow state
  -- runs the game logic
  _ <- forkIO $ L.startLogicLoop state
  -- updates the GUI
  _ <- forkIO $ G.update state buttons rs
  mainGUI
