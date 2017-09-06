module Logic where

import Data.Array
import Material (Player (..), Tile (..), Index, Board, Direction)
import qualified Material as M
import qualified Recommender as R
import Control.Concurrent

data GameState = Running {players::[Player]} | Draw | Won {winner :: Player}
data State = State {
        gameSt :: GameState
      , history :: History
      , board :: Board
      , playerFunctions :: PlayerFunctions
    }



type History = [(Index, Player)]
type GetMoveFunction = (History -> [Player] ->  Board -> IO Index)
type GetMoveFunctionDebug = (History -> Board -> [Player] -> IO (Index,[(Index,Int)]))
type PlayerFunctions = [(Player
                     , GetMoveFunction
                     , GetMoveFunctionDebug)]

possibleFunctions :: [(GetMoveFunction, String)]
possibleFunctions =
    [ (R.recom ,"Beatable AI")
    , (R.randI ,"Random AI")
    , (getMove ,"Human")
    ]

playerFunctionsStandard = [(Player 1, (R.randI, undefined)),(Player 2, (getMove, undefined))]
playersStandard = cycle [Player 1, Player 2]

loop :: IO (Board,[Player]) -> IO Board
loop inp = do
    (b, ps@(p:r)) <- inp
    print $ check5 b
    putStrLn $ M.simpleShow b
    --threadDelay 1000000
    let f = case lookup p playerFunctionsStandard
                of Just x -> fst x
                   Nothing -> getMove
    i <-  f [] ps b
    loop $ return (M.setTile i (ident p) b, r)

-- Gets a move by input trough the command line.
getMove :: History -> [Player] -> Board -> IO Index
getMove h ps b = getIndex b >>=
        (\i -> case M.inBounds i b >>= flip M.maybeEmpty b
               of Just ix -> return ix
                  Nothing -> getMove h ps b)
                                      
getIndex :: Board -> IO Index
getIndex b = do
    putStr "X: "
    x <- getLine
    putStr "Y: "
    y <- getLine
    return (read x :: Int, read y :: Int)
    
    
    
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
    startl = [(i,0)|i<-[0..(M.height-1)]]
    startr = [(i,M.width-1)|i<-[0..(M.height-1)]]
    startu = [(0,i)|i<-[0..(M.width-1)]]

    check5'' :: [Maybe Index]
    check5'' = map (check5inLine b f) is

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
