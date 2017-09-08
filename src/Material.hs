module Material where

import Data.Array

{-
  Player
-}
newtype Player = Player {ident :: Int}
  deriving (Show, Eq)


{-
  Board
-}

type Index = (Int,Int)
type Board = Array Index Tile
--Dimension of the Gameboard
width = 20
height = 20

data Tile = Empty 
          | Set {playerId::Int}
          deriving (Eq)

instance Show Tile where
    show Empty  = "Empty"
    show (Set p)= show p

          
data Direction = Horizontal
               | Vertical
               | Diagonall
               | Diagonalr
               deriving (Show, Eq, Enum)

allDirections = [Horizontal .. Diagonalr]

allIs :: Board -> [Index]
allIs b = range $ bounds b

-- Checks whether the index is in bounds
inBounds :: Index -> Board -> Maybe Index
inBounds i b = if inRange (bounds b) i then Just i else Nothing

-- Check whether the tile at the index is empty
maybeEmpty :: Index -> Board -> Maybe Index
maybeEmpty i b = if isEmpty b i then Just i else Nothing
               
-- Gives an empty board
emptyBoard :: Board
emptyBoard = array ((0,0),(h',w')) [((i,j),Empty)|j<-[0..h'],i<-[0..w']]
    where w' = width-1
          h' = height-1

emptyBoardL :: Board
emptyBoardL = array ((0,0),(h',w')) [((i,j),Empty)|j<-[0..h'],i<-[0..w']]
    where w' = width^2 - 1
          h' = height^2 - 1
          
filledBoard :: Player -> Board
filledBoard p = setBoard is emptyBoard 
    where t = Set $ ident p
          is = [(i,t)|i<-indices emptyBoard]

-- Checks whether the tile at the specified index is empty
isEmpty :: Board -> Index -> Bool
isEmpty b i = Empty==(b!i)

-- Checks whether there is any tile that is empty
anyEmpty :: Board -> Bool
anyEmpty b = any (isEmpty b) ixs
    where
      ixs = range $ bounds b

-- Gives all indexes where the board is empty
allEmptyIs :: Board -> [Index]
allEmptyIs b = filter ((==Empty).(b!)) (allIs b)
          
-- Trys to set the tiles at the specified indices
setBoard :: [(Index, Tile)] -> Board -> Board
setBoard = flip (//)

-- Trys to set the tile at the index
setTile i p = setBoard ls
    where ls = [(i, Set p)]

-- Is the tile set by this player?
tileSetBy :: Tile -> Player -> Bool
tileSetBy t p = ident p == playerId t

-- Gives the player
setBy :: Board -> Index -> Player
setBy b i = Player $ playerId (b ! i)
    
-- Checks if the tile at the index is set by the
-- same player                                 
setBySame :: Maybe Index -> Tile -> Board -> Bool
setBySame Nothing  _      _ = False
setBySame _        Empty  _ = False
setBySame (Just i) t      b = b ! i == t    

-- Shows the board as a sligthly formatted string
-- Shows the board as a sligthly formatted string
simpleShow :: Board -> String
simpleShow = (++"\n").reverse.fst.foldr (\x (out,n) 
        -> let s = out ++ "," ++ showT x in 
           if n < (width-1) then (s, n+1) else (s ++ "\n", 0)
        ) ("",0)
    where
      showT Empty = " "
      showT (Set p) = show p
      
-- Functions to get an adjacent index on the board
changeIndex :: (Int->Int,Int->Int) -> Index -> Index
changeIndex (f,g) (a,b) = (f a, g b)

up = changeIndex ((+(-1)),id)
down = changeIndex ((+1),id)
left = changeIndex (id,(+(-1)))
right = changeIndex (id,(+1))

-- Gives the pair of function to get adjacent indices in one direction
dirToFs :: Direction -> (Index->Index, Index->Index)
dirToFs d | d == Horizontal = (left, right)
          | d == Vertical = (up, down)
          | d == Diagonall = (up.right,down.left)
          | d == Diagonalr = (up.left,down.right)
          | otherwise = (id,id)
