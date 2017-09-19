module GUI
  (
    update
  , setWindow
  )
  where

import Graphics.UI.Gtk
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, foldM, liftM)
import Data.Array (assocs)
import Data.Text (pack)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map

import Material (Index, Board, Tile(..), Player(..))
import Logic (StateVars (..), tindex, tgame, tplayers)
import qualified Material as M
import qualified Logic as L

type LeftSide = (Fixed, Map.Map Index Button)
type RightSide = (Label, ComboBox, ComboBox)

-- Updates the GUI
update :: StateVars -> Map.Map Index Button -> RightSide -> IO()
update s bmap (l, _, _)= forever
    (atomically (do
        (L.Game _gst h b) <- readTVar (tgame s)
        buttonsU1 <- needUpdateWholeBoard bmap b
        buttonsU2 <- needUpdateH h bmap b
        let isNull1 = null buttonsU1
        let isNull2 = null buttonsU2
        if isNull1 && isNull2
        then retry
        else return (b, if isNull1 then buttonsU2 else buttonsU1)
      )
      >>= (\ (b,buttons) -> postGUIAsync $ updateButtons b buttons
      >> updateStatusLabel l s) >> threadDelay 100000
    )

-- Update status label
updateStatusLabel :: Label -> StateVars -> IO()
updateStatusLabel l s = do
  (L.Players (p:_) _,L.Game gst _ _) <- atomically $ do
    a <- readTVar $ tplayers s
    b <- readTVar $ tgame s
    return (a,b)
  let labelnew = case gst of
            L.Running -> let pindex = M.ident p in "Running: Player " ++
                            show pindex ++ if pindex == 1 then " (X)" else " (O)"
            L.Won p1 -> "Player " ++ show (M.ident $ fst p1) ++ " won!"
            L.Draw -> "Draw"
            L.New -> "Running"
  labelSetText l labelnew

-- Find indexes from the history that need to be
-- updated by comparing the value stored in the button 
needUpdateH :: L.History -> Map.Map Index Button -> Board -> STM [(Index,Button)]
needUpdateH [] _bmap _board = return []
needUpdateH ((i,_p):hs) bmap board = case M.getTile i board of
    Just t -> do
      let b' = Map.lookup i bmap
      case b' of 
        Nothing -> return []
        Just b-> do
          let blabel = unsafePerformIO $ buttonGetLabel b
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
needUpdateWholeBoard :: Map.Map Index Button -> Board -> STM [(Index,Button)]
needUpdateWholeBoard bmap board = do
    fs' <- sequence $ fs bmap
    if or fs'
    then return $ Map.toList bmap
    else return []
  where
    fs :: Map.Map Index Button -> [STM Bool]
    fs bmap' = map (\i -> case Map.lookup i bmap' of
                Just b -> do 
                    let blabel = unsafePerformIO $ buttonGetLabel b
                    return $ blabel /= " "
                Nothing -> return False) 
             (M.allEmptyIs board)

tileSigns :: [(M.Tile, String)]
tileSigns = [(M.Empty, " "),(M.Set 1, "X"),(M.Set 2, "O")]

-- Updates the labels on the buttons according to the board
updateButtons :: Board -> [(Index,Button)] -> IO()
updateButtons b bmap = mapM_ (changeLabel bmap) (assocs b)
  where
    changeLabel ::[(Index,Button)] -> (Index, Tile) -> IO()
    changeLabel bs (i,t) = do
      let button = lookup i bs
      case button of
        Just button' -> buttonSetLabel button' (lab t)
        Nothing -> return()
    lab (Set 1) = "X"
    lab (Set 2) = "O"
    lab _Empty = " "
    -- pLabels = ["","X","O"]

-- Creates the window and everything inside it.
-- Gives the window the left (Board) and the right (Settings and Overview) side of the GUI
setWindow :: StateVars -> IO (Window, LeftSide, RightSide)
setWindow s = do
    window <- windowNew
    vboxr <- vBoxNew False 0
    vbox <- vBoxNew False 0
    hbox <- hBoxNew False 0
    fixed <- fixedNew
    bs <- createBoard s fixed M.emptyBoard
    let buttonmap = Map.fromList bs
    set window [windowDefaultWidth := 500, windowDefaultHeight := 440,
                containerBorderWidth := 0, containerChild := hbox,
                windowTitle := "Has5"]
    _ <- onDestroy window mainQuit
    vsep <- vSeparatorNew
    hsep <- hSeparatorNew
    rs <- createRightSide vboxr s
    boxPackStart vbox fixed PackNatural 0
    boxPackStart vbox hsep PackNatural 0
    boxPackStart hbox vbox PackNatural 0
    boxPackStart hbox vsep PackNatural 0
    boxPackStart hbox vboxr PackGrow 0
    widgetShowAll window
    return (window, (fixed, buttonmap), rs)

-- Creates the buttons for the board
createBoard :: StateVars -> Fixed -> Board -> IO [(Index,Button)]
createBoard s fixed b = foldM (\o i -> do
      n <- boardButton s fixed i
      return ((i,n):o)) [] (M.allIs b)

-- Creates one button for the board
boardButton :: StateVars -> Fixed -> Index -> IO Button
boardButton  s fixed index@(x,y)= do
    b <- buttonNew
    _ <- onClicked b (boardButtonPress b s index) -- FIXME deprecated function onClicked
    fixedPut fixed b (y*24,x*24)
    widgetSetSizeRequest b 24 24
    return b

-- Handles the button presses
-- Sets the TVar to the index of the pressed button
boardButtonPress :: Button -> StateVars -> Index -> IO ()
boardButtonPress _b s i = return (putStrLn "buttonPressed") >> atomically $ writeTVar (tindex s) i

-- Create settings panel
-- Restart-Button, Labels,
createRightSide :: VBox -> StateVars -> IO RightSide
createRightSide vbox s = do
    fixed <- fixedNew
    widgetSetSizeRequest fixed 190 400
    labelm <- labelNew $ Just "Start:"
    hboxlabel <- hBoxNew False 0
    boxPackStart hboxlabel labelm PackNatural 0
    fixedPut fixed hboxlabel (35,30)
    hbox1 <- hBoxNew False 0
    label1 <- labelNew $ Just "Player 1: "
    comboBox1 <- comboBoxes s (M.Player 1) L.possibleFunctions
    boxPackStart hbox1 label1 PackNatural 0
    boxPackStart hbox1 comboBox1 PackNatural 0
    fixedPut fixed hbox1 (20, 60)
    hbox2 <- hBoxNew False 0
    label2 <- labelNew $ Just "Player 2: "
    comboBox2 <- comboBoxes s (M.Player 2) L.possibleFunctions
    boxPackStart hbox2 label2 PackNatural 0
    boxPackStart hbox2 comboBox2 PackNatural 0
    fixedPut fixed hbox2 (20, 80)
    hbox3 <- hBoxNew False 0
    buttonrestart <- buttonNewWithLabel "Restart"
    _ <- onClicked buttonrestart (restart buttonrestart s)
    boxPackStart hbox3 buttonrestart PackNatural 0
    fixedPut fixed hbox3 (60,120)
    boxPackStart vbox fixed PackNatural 0
    return (labelm, comboBox1, comboBox2)

-- restarts the game by resetting the game TVar
restart :: Button -> StateVars -> IO()
restart _b s = atomically $
  writeTVar (tgame s) L.gameDefault
    
-- gives a combo box that allows the user to swap the playerfunctions
comboBoxes :: StateVars -> Player -> [(String, L.MoveFunction)] -> IO ComboBox
comboBoxes s p inp= do
    cb <- comboBoxNewText
    mapM_  (comboBoxAppendText cb . pack.fst) inp
    _ <- onChanged cb (cbChanged s p (comboBoxGetActive cb)) -- FIXME deprecated Function onChanged
    comboBoxSetActive cb 0
    return cb

-- Updates the index indicating the function for the player
cbChanged :: StateVars -> Player -> IO Int-> IO()
cbChanged s p i' = do
    i <- i'
    if i < 0
    then return ()
    else atomically $ change i
  where
    change i = do
      (L.Players pc ps) <- readTVar $ tplayers s
      writeTVar (tplayers s) (L.Players pc (changeTo i p ps))
    changeTo :: Int -> Player -> [(Player, Int)] -> [(Player, Int)]
    changeTo i p0 = map (\p2 -> if fst p2 == p0 then (p0,i) else p2)
