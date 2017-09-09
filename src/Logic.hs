module Logic where

import Data.Array
import Material (Player (..), Tile (..), Index, Board, Direction)
import qualified Material as M
import qualified Recommender as R
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Parallel.Strategies
import Control.Monad


type History = [(Index, Player)]
type GetMoveFunction = (TVar Index -> History -> [Player] ->  Board -> IO Index)
type PlayerFunctions = [(Player, GetMoveFunction)]

data GameState = New | Running | Draw | Won {winner :: (Player, (Index,Direction))}
    deriving (Eq, Show)

data Game = Game {
        gameSt :: GameState
      , history :: History
      , board :: Board
    }
    deriving (Eq)

-- Make Player Order a seperate type and try to synchronize it with something else than STM vielleicht eine MVar?
data Players = Players {playerOrder :: [Player], pfindex :: [(Player,Int)]}
  deriving (Eq,Show)

-- A new game. Used on start and for restarting.
gameDefault :: Game
gameDefault =
  Game {gameSt = New
      , history = []
      , board = M.emptyBoard}

-- The default settings for players.
playersDefault :: Players
playersDefault =
  Players {playerOrder = cycle [p1, p2]
         , pfindex = [(p1, 0),(p2, 0)]}
  where p1 = Player 1
        p2 = Player 2

-- The functions that can be used to get a Players move
-- and their String representation for ComboBoxes in the GUI
possibleFunctions :: [(String, GetMoveFunction)]
possibleFunctions =
    [ ("Human",getMoveH) -- Change to getMoveH if supposed to start with GUI otherwise getMoveP
    , ("Beatable AI",recom)
    , ("Random AI", randI)
    ]

-- Old logic, updated logic under logic2
logic :: TVar Players -> TVar Game -> TVar Index -> IO ()
logic players game index = forever $ do
    p@(Players ps pf) <- readTVarIO players
    g@(Game gst' h b) <- readTVarIO game
    let gst = checkGameSt (check5 b)  b
    putStrLn $ M.simpleShow b
    putStrLn $ "GameState: " ++ show gst'
    if gst' == Running then -- if it'
     do
       i <- (f ps pf) index h ps b -- evaluating the gamestate after asking for the index
                                   -- leads to a bug where the player is asked for input alltough
                                   -- the state shouldnt be Running anymore
                                   -- could potentially lead to bugs for a draw when
                                   -- there are no empty spaces left
       print i
       atomically $ setTVars gst p g i game players
       putStrLn "Was set"
       return ()
    else
      putStrLn "Nothing"
    threadDelay 1000000
    putStrLn "Looped"
  where
    f (p:_) pf = case lookup p pf
                   of Just x -> snd $ possibleFunctions !! x
                      Nothing -> snd $ head possibleFunctions

    -- Sets the index only if the state of the tvars are the same as in the beginning of the logic function
    setTVars :: GameState -> Players -> Game -> Index -> TVar Game -> TVar Players -> STM()
    setTVars gst' (Players p' _) g' i game players = do
        (Players p@(p0:r) pf) <- readTVar players
        g@(Game gst h b) <- readTVar game
        if gst' /= gst then
          writeTVar game (Game gst' h b)
        else
         do
          writeTVar players (Players r pf)
          let b' = M.setTile i (ident p0) b
          writeTVar game (Game gst' h b')

-- Actual loop for the logic
-- TODO Change type to (TVar Index -> History -> [Player] ->  Board -> STM Index) and replace calls to Random in Recommmender
-- Maybe can be cleaned up by removing the need for IO in GetMoveFunctions
-- would need another Random Number Generator or the use of unsafePerformIO
-- Would probably make the STM work better by not having to use readTVarIO
-- and thus being able to execute the whole loop in STM
logic2 :: TVar Players -> TVar Game -> TVar Index -> IO ()
logic2 players game index = forever $ do
    -- Check whether the game has been won or reset
    atomically $ setGameStateInTVar game
    g@(Game gst h b) <- readTVarIO game
    --putStrLn $ M.simpleShow b
    --putStrLn $ "GameState: " ++ show gst
    case gst of
      New -> atomically (setGameStateInTVar game)
      Won p -> return ()
      Draw -> return ()
      Running -> do
        p@(Players pc@(p0:r) pfi) <- readTVarIO players
        let pfunction = findFunction pc pfi
        i <- pfunction index h pc b
        let b' = M.setTile i (ident p0) b
        case M.inBounds i b >>= flip M.maybeEmpty b of
          Nothing -> return ()
          Just i -> atomically $ do
            b <- hasChanged g p game players
            if b
            then return()
            else writeTVar game (Game gst ((i,p0):h) b') >>
                            writeTVar players (Players r pfi)
    threadDelay 100000 -- Pauses till the next execution. Otherwise the moves by the AI are easily overlooked.
  where
    findFunction (p:_) pf = case lookup p pf of
                  Just x -> snd $ possibleFunctions !! x
                  Nothing -> snd $ head possibleFunctions
    hasChanged :: Game -> Players -> TVar Game -> TVar Players -> STM Bool
    hasChanged g p game player = do
        g' <- readTVar game
        p' <- readTVar player
        return False -- $ g == g' && p' == p


-- Sets the GameState in the TVar after checking whether it should be changed
setGameStateInTVar :: TVar Game -> STM()
setGameStateInTVar game = do
    (Game gst h b) <- readTVar game
    if gst /= New
    then do
      let new = checkGameSt (check5 b) b
      writeTVar game (Game new h b)
    else
      writeTVar game (Game Running h b)


-- Checks whether the game is over or still ongoing
checkGameSt :: [(Index, Direction)] -> Board -> GameState
checkGameSt []    b | M.anyEmpty b =  Running
                    | otherwise = Draw
checkGameSt (x:_) b = Won {winner = (M.setBy b (fst x),x)}

-- Gets the next button press by the user as input
getMoveH :: TVar Index -> History -> [Player] -> Board -> IO Index
getMoveH varI h ps b = do
      --threadDelay 1000000
      index <- readTVarIO varI
      if index == (-1,-1)
      then atomically $ setM >> return index
      else return index
    where
      setM = writeTVar varI (-1,-1)

-- Gets a move by querying input trough the command line.
getMoveP :: TVar Index -> History -> [Player] -> Board -> IO Index
getMoveP t h ps b = getIndex b >>=
        (\i -> case M.inBounds i b >>= flip M.maybeEmpty b
               of Just ix -> return ix
                  Nothing -> getMoveP t h ps b)
    where
      getIndex :: Board -> IO Index
      getIndex b = do
          y <- getLine
          x <- getLine
          return (read y :: Int, read x :: Int)

-- Aliases for Recommender functions that also throw away the not needed TVar
recom :: TVar Index -> History -> [Player] -> Board -> IO Index
recom t = R.recom

randI :: TVar Index -> History -> [Player] -> Board -> IO Index
randI t = R.randI

-- Checks whether a Player has connected 5.
-- Applies the checking functions to all directions
-- Gives a list of the starting indices and the direction.
check5 :: Board -> [(Index, Direction)]
check5 b = concat $ [check5' b]<*>M.allDirections

-- Gives a list of the starting indices of 5 connected for one direction.
check5' :: Board -> Direction -> [(Index, Direction)]
check5' b d =
    foldr (\x out-> case x of
                Nothing -> out
                Just y -> (y,d) : out
          ) [] check5''
  where
    check5'' :: [Maybe Index]
    check5'' = map (check5inLine b f) is -- `using` parList rdeepseq -- takes half the time it would without it.
    -- but only measurable on very large boards( 400*400 takes ~ 0,026 disabled and ~ 0,013 enabled) therefore disabled
    -- For bigger Boards it would be effective

    (f,is) = case d of
        M.Horizontal -> (M.right,startl)
        M.Vertical   -> (M.down, startu)
        M.Diagonall  -> (M.down.M.left,startu++tail startr)
        M.Diagonalr  -> (M.down.M.right,startu++tail startl)

    -- Indices on the border of the board (left, right and up)
    startl = [(i,0)|i<-[0..(snd (snd (bounds b)) - 1)]]
    startr = [(i,fst (snd (bounds b)) -1)|i<-[0..(snd (snd (bounds b))-1)]]
    startu = [(0,i)|i<-[0..(snd (snd (bounds b))-1)]]

-- Checks one line for 5 in a row.
check5inLine :: Board -> (Index -> Index) -> Index -> Maybe Index
check5inLine b = ch (Nothing, Empty, 0)
    where ch :: (Maybe Index,Tile,Int) -> (Index->Index) -> Index -> Maybe Index
          ch (mi,t,5) _f _i = mi
          ch (mi,t,n) f i   | a@(Just ix) <- inBounds' i
                                    = if M.setBySame a t b
                                        then ch (mi,t,n+1) f (f ix)
                                        else ch (Just i,b!i,1) f (f ix)
                                | otherwise
                                    = Nothing
          inBounds' = flip M.inBounds b
