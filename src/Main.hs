module Main where

import Control.Concurrent
import Control.Monad (void, forever)
import System.Exit
import Control.Concurrent.STM.TVar

import qualified Logic as L
import qualified Material as M

main = do
  game <- newTVarIO L.gameDefault
  players <- newTVarIO L.playersDefault
  --forkIO startLoop
  forkIO $ startLogicLoop game players
  threadDelay 1000000000
  --exitSuccess


startLogicLoop :: TVar L.Game -> TVar L.Players -> IO()
startLogicLoop g p = void $ L.logic (return p) (return g)


{- -- Moved to startLogicLoop
startLoop :: IO ()
startLoop = void (L.loop $ return (M.emptyBoard, L.playersStandard))
-}
