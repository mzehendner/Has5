module GUI where

import Graphics.UI.Gtk
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Material (Index, Board, Tile(..))
import qualified Material as M
import qualified Logic as L
import Data.Array (assocs)

type LeftSide = (Fixed, [(Index, Button)])

update :: TVar L.Game -> TVar L.Players -> [(Index,Button)] -> IO()
update game' players' bmap = forever $ do
    game@(L.Game gst h b) <- readTVarIO game'
    players <- readTVarIO players'
    postGUIAsync $ updateButtons b bmap
    threadDelay 50000
    return ()

updateButtons ::Board -> [(Index,Button)] -> IO()
updateButtons b bmap = mapM_ (changeLabel bmap) (assocs b)
  where
    changeLabel ::[(Index,Button)] -> (Index, Tile) -> IO()
    changeLabel bs (i,t) = do
      let button = lookup i bs
      case button of
        Just button' -> buttonSetLabel button' (lab t)
        Nothing -> return()
    lab Empty = " "
    lab (Set 1) = "X"
    lab (Set 2) = "O"
    -- pLabels = ["","X","O"]

setWindow :: TVar Index -> IO (Window, LeftSide)
setWindow varI = do
    window <- windowNew
    vbox <- vBoxNew False 0
    hbox <- hBoxNew False 0
    fixed <- fixedNew
    bs <- createBoard varI fixed M.emptyBoard
    set window [windowDefaultWidth := 500, windowDefaultHeight := 400,
                    containerBorderWidth := 0, containerChild := hbox]
    onDestroy window mainQuit
    vsep <- vSeparatorNew
    hsep <- hSeparatorNew
    boxPackStart vbox fixed PackNatural 0
    boxPackStart vbox hsep PackNatural 0
    boxPackStart hbox vbox PackNatural 0
    boxPackStart hbox vsep PackNatural 0
    widgetShowAll window
    return (window, (fixed, bs))

-- Creates the buttons for the board
createBoard :: TVar Index -> Fixed -> Board -> IO [(Index,Button)]
createBoard varI fixed b =
{-    foldr (\i out -> do n <- oneButton varI fixed i
                        o <- out
                        return (n:o))
          (return [])
          (M.allIs b)
-}
    foldM (\o i -> do n <- oneButton varI fixed i
                      return ((i,n):o))
          []
          (M.allIs b)

-- Creates one button completely
oneButton ::TVar Index -> Fixed -> Index -> IO Button
oneButton  varI fixed index@(x,y)= do
    b <- buttonNew
    onClicked b (buttonPress b varI index)
    fixedPut fixed b (y*20,x*20)
    widgetSetSizeRequest b 20 20
    return b

-- Handles the button presses
-- Sets the TVar to the index of the pressed button
buttonPress :: Button -> TVar Index -> Index -> IO ()
buttonPress b varI i = do
    atomically setChosenIndex
    buttonSetLabel b "x"
  where
    setChosenIndex :: STM ()
    setChosenIndex = writeTVar varI i