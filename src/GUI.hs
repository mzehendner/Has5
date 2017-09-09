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

-- updates the GUI
update :: TVar L.Game -> TVar L.Players -> [(Index,Button)] -> RightSide -> IO()
update game' players' bmap (l, _, _)= forever $ do
    game@(L.Game gst h b) <- readTVarIO game'
    players <- readTVarIO players'
    --Check whole board first then history
    buttonsU <- needUpdateWholeBoard bmap b
    buttonsU' <- if length buttonsU == 0 
                 then needUpdateH h bmap b
                 else return buttonsU
    postGUIAsync $ updateButtons b buttonsU' 
        >> updateStatusLabel l players' game'
    threadDelay 50000
    -- as few and short postGUIAsync calls as possible
    -- otherwise the gui gets unresponsive    
    
-- Update status label
updateStatusLabel :: Label -> TVar L.Players -> TVar L.Game -> IO()
updateStatusLabel l players game = do
  (L.Players (p:_) _)<- readTVarIO players
  (L.Game gst _ _) <- readTVarIO game
  let s = case gst of
            L.Running -> "Running: Player " ++ show (M.ident p)
            L.Won p1 -> "Player " ++ show (M.ident $ fst p1) ++ " won!"
            L.Draw -> "Draw"
            L.New -> "Running"
  labelSetText l s

-- Find indexes from the history that need to be
-- updated by comparing the value stored in the button 
needUpdateH :: L.History -> [(Index,Button)] -> Board -> IO [(Index,Button)]
needUpdateH [] bmap board = return []
needUpdateH _  []   _ = return []
needUpdateH ((i,p):hs) bmap board = case M.getTile i board of 
    Just t -> do
      let b' = lookup i bmap
      case b' of 
        Nothing -> return []
        Just b-> do
          blabel <- buttonGetLabel b
          if different t blabel
          then do
            next <- needUpdateH hs bmap board
            return $ (i,b) : next
          else return []
    Nothing -> needUpdateH hs bmap board
  where
    different :: Tile -> String -> Bool
    different t blabel = lookup t tileSigns /= Just blabel

-- If any empty tile on the board is not set to " " 
-- update all buttons.
-- Removes the slowdown after a restart
needUpdateWholeBoard :: [(Index,Button)] -> Board -> IO [(Index,Button)]
needUpdateWholeBoard bmap board = do
    fs' <- sequence $ fs bmap
    if any id fs'
    then return bmap 
    else return []
  where
    fs :: [(Index, Button)] -> [IO Bool]
    fs bmap' = map (\i -> case lookup i bmap' of 
                Just b -> do 
                    blabel <- buttonGetLabel b 
                    return $ blabel /= " "
                Nothing -> return False) 
             (M.allEmptyIs board)

tileSigns = [(M.Empty, " "),(M.Set 1, "X"),(M.Set 2, "O")]

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
    set window [windowDefaultWidth := 500, windowDefaultHeight := 440,
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

-- Creates one button for the board
boardButton ::TVar Index -> Fixed -> Index -> IO Button
boardButton  varI fixed index@(x,y)= do
    b <- buttonNew
    onClicked b (boardButtonPress b varI index) -- FIXME deprecated function onClicked
    fixedPut fixed b (y*24,x*24)
    widgetSetSizeRequest b 24 24
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

-- restarts the game by resetting the TVars
restart :: Button -> TVar L.Game -> TVar L.Players -> TVar Index -> IO()
restart b game players index = atomically $ do
    (L.Players _ pc) <- readTVar players
    writeTVar players (L.playersDefault {L.pfindex = pc}) >>
      writeTVar game L.gameDefault >>
      writeTVar index (-1,-1)
    
-- gives a combo box that allows the user to swap the playerfunctions
comboBoxes :: TVar L.Players -> Player -> [(String, L.GetMoveFunction)] -> IO ComboBox
comboBoxes ps p inp= do
    cb <- comboBoxNewText
    mapM_  (comboBoxAppendText cb .T.pack.fst) inp
    onChanged cb (cbChanged ps p (comboBoxGetActive cb)) -- FIXME deprecated Function onChanged
    comboBoxSetActive cb 0
    return cb

-- Updates the index indicating the function for the player
cbChanged :: TVar L.Players -> Player -> IO Int-> IO()
cbChanged players p i' = do
    i <- i'
    if i < 0
    then return ()
    else atomically $ change i
  where
    change i = do
      (L.Players pc ps) <- readTVar players
      writeTVar players (L.Players pc (changeTo i p ps))
    changeTo :: Int -> Player -> [(Player, Int)] -> [(Player, Int)]
    changeTo i p0 = map (\p2 -> if fst p2 == p0 then (p0,i) else p2)
