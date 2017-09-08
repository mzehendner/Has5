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
type GetMoveFunction = (History -> [Player] ->  Board -> IO Index)

-- TODO add functionality for a debug function
type GetMoveFunctionDebug = (History -> Board -> [Player] -> IO (Index,[(Index,Int)]))
type PlayerFunctions = [(Player
                     , (GetMoveFunction
                     , GetMoveFunctionDebug))]

data GameState = New | Running | Draw | Won {winner :: (Player, (Index,Direction))}
    deriving (Eq, Show)

data Game = Game {
        gameSt :: GameState
      , history :: History
      , board :: Board
    }
    deriving (Eq)

-- Make Player Order a seperate type and try to synchronize it with something else than STM vielleicht eine MVar?
data Players = Players {playerOrder :: [Player], pfindex :: [(Player,(Int, Int))]}

gameDefault :: Game
gameDefault =
  Game {gameSt = Running
      , history = []
      , board = M.emptyBoard}

playersDefault :: Players
playersDefault =
  Players {playerOrder = cycle [p1, p2]
         , pfindex = [(p1, (0, undefined)),(p2, (2, undefined))]}
  where p1 = Player 1
        p2 = Player 2

possibleFunctions :: [(GetMoveFunction, String)]
possibleFunctions =
    [ (getMoveP ,"Human")
    , (R.recom ,"Beatable AI")
    , (R.randI ,"Random AI")
    ]



{- -- Old loop without concurrency
playerFunctionsStandard = [(Player 1, (R.recom, undefined)),(Player 2, (R.recom, undefined))]
playersStandard = cycle [Player 1, Player 2]

loop :: IO (Board,[Player]) -> IO Board
loop inp = do
    (b, ps@(p:r)) <- inp
    print $ check5 b
    putStrLn $ M.simpleShow b
    --threadDelay 1000000
    let f = case lookup p playerFunctionsStandard
                of Just x -> fst x
                   Nothing -> getMoveP
    i <-  f [] ps b
    loop $ return (M.setTile i (ident p) b, r)
-}

logic :: IO (TVar Players) ->IO (TVar Game) -> IO ()
logic players game = forever $ do
    players' <- players
    game' <- game
    p@(Players ps pf) <- readTVarIO players'
    g@(Game gst' h b) <- readTVarIO game'
    let gst = setGameSt (check5 b)  b
    putStrLn $ M.simpleShow b
    putStrLn $ "GameState: " ++ show gst'
    if gst' == Running then -- if it'
     do
       i <- (f ps pf) h ps b -- evaluating the gamestate after asking for the index
                             -- leads to a bug where the player is asked for input alltough
                             -- the state shouldnt be running anymore
                             -- could potentially lead to bugs for a draw
       print i
       atomically $ setTVars gst p g i game' players'
       putStrLn "Was set"
       return ()
    else
      putStrLn "Nothing"
    threadDelay 1000000
    putStrLn "Looped"
  where
    f (p:_) pf = case lookup p pf
                   of Just x -> fst $ possibleFunctions !! fst x
                      Nothing -> fst $ head possibleFunctions

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

-- Checks whether the game is over or still ongoing
setGameSt :: [(Index, Direction)] -> Board -> GameState
setGameSt []    b | M.anyEmpty b =  Running
                  | otherwise = Draw
setGameSt (x:_) b = Won {winner = (M.setBy b (fst x),x)}

-- Gets a move by input trough the command line.
getMoveP :: History -> [Player] -> Board -> IO Index
getMoveP h ps b = getIndex b >>=
        (\i -> case M.inBounds i b >>= flip M.maybeEmpty b
               of Just ix -> return ix
                  Nothing -> getMoveP h ps b)

getIndex :: Board -> IO Index
getIndex b = do
    y <- getLine
    x <- getLine
    return (read y :: Int, read x :: Int)



-- Checks whether a Player has connected 5.
-- Give a list of the starting indices and the direction.
check5 :: Board -> [(Index, Direction)]
check5 b = concat $ [check5' b]<*>M.allDirections

-- Gives a list of the indices for one direction.
check5' :: Board -> Direction -> [(Index, Direction)]
check5' b d =
    foldr (\x out-> case x of
                Nothing -> out
                Just y -> (y,d) : out
          ) [] check5''
  where
    (f,is) = case d of
        M.Horizontal -> (M.right,startl)
        M.Vertical   -> (M.down, startu)
        M.Diagonall  -> (M.down.M.left,startu++tail startr)
        M.Diagonalr  -> (M.down.M.right,startu++tail startl)
    startl = [(i,0)|i<-[0..(snd (snd (bounds b)) - 1)]]
    startr = [(i,fst (snd (bounds b)) -1)|i<-[0..(snd (snd (bounds b))-1)]]
    startu = [(0,i)|i<-[0..(snd (snd (bounds b))-1)]]

    check5'' :: [Maybe Index]
    check5'' = map (check5inLine b f) is -- `using` parList rdeepseq -- takes half the time it would without it.
    -- but only measurable on very large boards 400*400 takes ~ 0,026 disabled and ~ 0,013 enabled
    -- if the board was bigger it would be effective

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
