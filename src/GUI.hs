module GUI where

import Graphics.UI.Gtk
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Array (assocs)
import qualified Data.Text as T

import Material (Index, Board, Tile(..), Player(..))
import qualified Material as M
import qualified Logic as L

type LeftSide = (Fixed, [(Index, Button)])
type RightSide = (Label, ComboBox, ComboBox)

update :: TVar L.Game -> TVar L.Players -> [(Index,Button)] -> RightSide -> IO()
update game' players' bmap (l, _, _)= forever $ do
    game@(L.Game gst h b) <- readTVarIO game'
    players <- readTVarIO players'
    postGUIAsync $ updateButtons b bmap
    postGUIAsync $ updateStatusLabel l players' game'
    threadDelay 50000
    return ()

-- Update status label
updateStatusLabel :: Label -> TVar L.Players -> TVar L.Game -> IO()
updateStatusLabel l players game = do
  (L.Players (p:_) _)<- readTVarIO players
  (L.Game gst _ _) <- readTVarIO game
  let s = case gst of
            L.Running -> "Running: " ++ show p
            L.Won p1 -> "Player " ++ show (M.ident $ fst p1) ++ " won!"
            Draw -> "Draw"
            New -> "Running"
  labelSetText l s

-- Updates the labels on the buttons according to the board
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

-- Creates the window and everything inside it.
-- Gives the window the left (Board) and the right (Settings and Overview) side of the GUI
setWindow :: TVar L.Game -> TVar L.Players -> TVar Index -> IO (Window, LeftSide, RightSide)
setWindow game players index = do
    window <- windowNew
    vboxr <- vBoxNew False 0
    vbox <- vBoxNew False 0
    hbox <- hBoxNew False 0
    fixed <- fixedNew
    bs <- createBoard index fixed M.emptyBoard
    set window [windowDefaultWidth := 500, windowDefaultHeight := 400,
                containerBorderWidth := 0, containerChild := hbox,
                windowTitle := "Has5"]
    onDestroy window mainQuit
    vsep <- vSeparatorNew
    hsep <- hSeparatorNew
    rs <- createRightSide vboxr game players index
    boxPackStart vbox fixed PackNatural 0
    boxPackStart vbox hsep PackNatural 0
    boxPackStart hbox vbox PackNatural 0
    boxPackStart hbox vsep PackNatural 0
    boxPackStart hbox vboxr PackGrow 0
    widgetShowAll window
    return (window, (fixed, bs), rs)

-- Creates the buttons for the board
createBoard :: TVar Index -> Fixed -> Board -> IO [(Index,Button)]
createBoard varI fixed b =
{-    foldr (\i out -> do n <- oneButton varI fixed i
                        o <- out
                        return (n:o))
          (return [])
          (M.allIs b)
-}
    foldM (\o i -> do n <- boardButton varI fixed i
                      return ((i,n):o))
          []
          (M.allIs b)

-- Creates one button completely
boardButton ::TVar Index -> Fixed -> Index -> IO Button
boardButton  varI fixed index@(x,y)= do
    b <- buttonNew
    onClicked b (boardButtonPress b varI index) -- FIXME deprecated function onClicked
    fixedPut fixed b (y*20,x*20)
    widgetSetSizeRequest b 20 20
    return b

-- Handles the button presses
-- Sets the TVar to the index of the pressed button
boardButtonPress :: Button -> TVar Index -> Index -> IO ()
boardButtonPress b varI i = atomically $ writeTVar varI i

-- Create settings panel
-- Restart-Button, Labels,
createRightSide :: VBox -> TVar L.Game -> TVar L.Players -> TVar Index -> IO RightSide
createRightSide vbox game players index = do
    labelm <- labelNew $ Just "Running: Player 1"
    hboxlabel <- hBoxNew False 0
    boxPackStart hboxlabel labelm PackNatural 0
    hbox1 <- hBoxNew False 0
    label1 <- labelNew $ Just "Player 1: "
    comboBox1 <- comboBoxes players (M.Player 1) L.possibleFunctions
    boxPackStart hbox1 label1 PackNatural 0
    boxPackStart hbox1 comboBox1 PackNatural 0
    hbox2 <- hBoxNew False 0
    label2 <- labelNew $ Just "Player 2: "
    comboBox2 <- comboBoxes players (M.Player 2) L.possibleFunctions
    boxPackStart hbox2 label2 PackNatural 0
    boxPackStart hbox2 comboBox2 PackNatural 0
    boxPackStart vbox hboxlabel PackNatural 0
    boxPackStart vbox hbox1 PackNatural 0
    boxPackStart vbox hbox2 PackNatural 0
    hbox3 <- hBoxNew False 0
    buttonrestart <- buttonNewWithLabel "Restart"
    onClicked buttonrestart (restart buttonrestart game players index)
    boxPackStart hbox3 buttonrestart PackNatural 0
    boxPackStart vbox hbox3 PackNatural 0
    return (labelm, comboBox1, comboBox2)

restart :: Button -> TVar L.Game -> TVar L.Players -> TVar Index -> IO()
restart b game players index = atomically $ do
    (L.Players _ pc) <- readTVar players
    writeTVar players (L.playersDefault {L.pfindex = pc}) >>
      writeTVar game L.gameDefault >>
      writeTVar index (-1,-1)

comboBoxes :: TVar L.Players -> Player -> [(String, L.GetMoveFunction)] -> IO ComboBox
comboBoxes ps p inp= do
    cb <- comboBoxNewText
    mapM_  ((comboBoxAppendText cb).T.pack.fst) inp
    onChanged cb (cbChanged ps p (comboBoxGetActive cb)) -- FIXME deprecated Function onChanged
    comboBoxSetActive cb 0
    return cb

cbChanged :: TVar L.Players -> Player -> IO Int-> IO()
cbChanged players p i' = do
    i <- i'
    print i
    if i < 0
    then return ()
    else atomically $ change i
  where
    change i = do
      (L.Players pc ps) <- readTVar players
      writeTVar players (L.Players pc (changeTo i p ps))
    changeTo :: Int -> Player -> [(Player, Int)] -> [(Player, Int)]
    changeTo i p0 = map (\p2 -> if fst p2 == p0 then (p0,i) else p2)

{-
  playersDefault :: Players
  playersDefault =
    Players {playerOrder = cycle [p1, p2]
           , pfindex = [(p1, 2),(p2, 0)]}
    where p1 = Player 1
          p2 = Player 2

  possibleFunctions :: [(GetMoveFunction, String)]
  possibleFunctions =
      [ (getMoveH ,"Human") -- Change to getMoveH if supposed to start with GUI otherwise getMoveP
      , (recom ,"Beatable AI")
      , (randI ,"Random AI")
      ]
-}