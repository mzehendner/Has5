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
type PlayerFunctions = [(Player
                     , (History -> [Player] ->  Board -> IO Index
                     , History -> Board -> [Player] -> IO [(Index,Int)]))]

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
                   Nothing -> getMove
    i <-  f [] ps b
    loop $ return (M.setTile i (ident p) b, r)
    
getMove :: History -> [Player] -> Board -> IO Index
getMove h ps b = getIndex b >>=
        (\i -> case M.inBounds i b of Just ix -> return ix
                                      Nothing -> getMove h ps b)
                                      
getIndex :: Board -> IO Index
getIndex b = do
    putStr "X: "
    x <- getLine
    putStr "Y: "
    y <- getLine
    return (read x :: Int, read y :: Int)
    
    
    
-- Checks whether a Player has connected 5
check5 :: Board -> [(Index, Direction)]
check5 b = concat $ [check5a b]<*>M.allDirections

check5a :: Board -> Direction -> [(Index, Direction)]
check5a b d = 
    foldr (\x out-> case x of 
                Nothing -> out
                Just y -> (y,d) : out
          ) [] ch
  where ch = check5b b f is
        (f,is) = case d of 
            M.Horizontal -> (M.right,startl)
            M.Vertical   -> (M.down, startu)
            M.Diagonall  -> (M.down.M.left,startu++tail startr)
            M.Diagonalr  -> (M.down.M.right,startu++tail startl)
        startl = [(i,0)|i<-[0..(M.height-1)]]
        startr = [(i,M.width-1)|i<-[0..(M.height-1)]]
        startu = [(0,i)|i<-[0..(M.width-1)]]

check5b :: Board -> (Index->Index)->[Index] -> [Maybe Index]
check5b b f = map (check5inLine b f)                  

check5inLine :: Board -> (Index -> Index) -> Index -> Maybe Index
check5inLine b = check' (Nothing, Empty, 0)
    where check' :: (Maybe Index,Tile,Int) -> (Index->Index) -> Index -> Maybe Index
          check' (mi,t,5) _f _i = mi
          check' (mi,t,n) f i   | a@(Just ix) <- inBounds' i
                                    = if M.setBySame a t b
                                        then check' (mi,t,n+1) f (f ix)
                                        else check' (Just i,b!i,1) f (f ix)
                                | otherwise 
                                    = Nothing      
          inBounds' = flip M.inBounds b
          
