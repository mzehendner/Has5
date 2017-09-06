module Main where

import Control.Concurrent
import Control.Monad (void)
import System.Exit

import qualified Logic as L
import qualified Material as M

main = do
  forkIO startLoop
  threadDelay 1000000
  exitSuccess

--void (forkIO startLoop >> threadDelay 1000000)

startLoop :: IO ()
startLoop = void (L.loop $ return (M.emptyBoard, L.playersStandard))