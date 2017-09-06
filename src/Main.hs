module Main where

import qualified Logic as L
import qualified Material as M

main = L.loop $ return (M.emptyBoard, L.playersStandard)