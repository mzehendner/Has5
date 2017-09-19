module Logic
  (
    Game (..)
  , GameState (..)
  , StateVars(..)
  , Players (..)
  , History
  , MoveFunction
  , possibleFunctions
  , gameDefault
  , playersDefault
  , startLogicLoop
  )
  where

import Data.Array
import Material (Player (..), Tile (..), Index, Board, Direction)
import qualified Material as M
import qualified Recommender as R
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)


type History = [(Index, Player)]
type MoveFunction = (Index -> History -> [Player] ->  Board -> STM Index)

data GameState = New | Running | Draw | Won {winner :: (Player, (Index,Direction))}
    deriving (Eq, Show)

data Game = Game {
        gameSt :: GameState
      , history :: History
      , board :: Board
    }
    deriving (Eq)

instance Show Game where
  show (Game gst hs _) = show gst ++ " " ++ show hs

data Players = Players {playerOrder :: [Player], pfindex :: [(Player,Int)]}
  deriving (Show)

data StateVars = StateVars {tplayers :: TVar Players, tgame :: TVar Game, tindex :: TVar Index}

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
possibleFunctions :: [(String, MoveFunction)]
possibleFunctions =
    [ ("Human",getMoveH)
    , ("Beatable AI",R.recom)
    --, ("Random AI", randI)
    ]

-- Starts the loop
startLogicLoop :: StateVars -> IO ()
startLogicLoop s = forever $ threadDelay 100000 >> atomically (logic s)

-- cleaned logic
logic :: StateVars -> STM ()
logic s = {--forever $ threadDelay 100000 >> atomically (--} do
    (Game gst _hs b) <- readTVar $ tgame s
    let gamestate = checkGameSt' gst (check5 b) b
    case gamestate of
      Draw -> retry
      Won _ -> retry
      New -> reset s
      Running -> move s
  where
    reset :: StateVars -> STM ()
    reset s = do
      writeTVar (tgame s) (Game Running [] M.emptyBoard)-- gameDefault {gameSt = Running}
      (Players _ pfis) <- readTVar (tplayers s)
      writeTVar (tplayers s) playersDefault {pfindex = pfis}
      writeTVar (tindex s) (-1,-1)

    move :: StateVars -> STM ()
    move s = do
      players@(Players pa@(p:ps) _pf) <- readTVar $ tplayers s
      game@(Game _ hs b) <- readTVar $ tgame s
      index <- readTVar $ tindex s
      let f = findFunction players
      indexNew <- f index hs pa b
      case M.inBounds (Just indexNew) b >>= flip M.maybeEmpty b of
        Nothing -> retry
        Just i -> do
          let b' = M.setTile i (ident p) b
          writeTVar (tgame s) (game {gameSt = checkGameSt (check5 b') b', history = (i, p):hs, board = M.setTile i (ident p) b})
          writeTVar (tplayers s) (players {playerOrder = ps})
          writeTVar (tindex s) (-1,-1)

    -- (Index -> Game -> Players -> STM Index)
    findFunction :: Players -> MoveFunction
    findFunction (Players (p:_) pf) = case lookup p pf of
                      Just x -> snd $ possibleFunctions !! x
                      Nothing -> snd $ head possibleFunctions

-- Checks whether the game is over, still running or new
checkGameSt' :: GameState -> [(Index, Direction)] -> Board -> GameState
checkGameSt' New _ _ = New
checkGameSt' _   x y = checkGameSt x y

-- Checks whether the game is over or still ongoing
checkGameSt :: [(Index, Direction)] -> Board -> GameState
checkGameSt []    b | M.anyEmpty b =  Running
                    | otherwise = Draw
checkGameSt (x:_) b = Won {winner = (M.setBy b (fst x),x)}

getMoveH :: Index -> History -> [Player] -> Board -> STM Index
getMoveH i _ _ _ = return i

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
          ch (mi,_t,5) _f _i = mi
          ch (mi,t,n) f i   | a@(Just ix) <- inBounds' (Just i)
                                    = if M.setBySame a t b
                                        then ch (mi,t,n+1) f (f ix)
                                        else ch (Just i,b!i,1) f (f ix)
                                | otherwise
                                    = Nothing
          inBounds' = flip M.inBounds b
