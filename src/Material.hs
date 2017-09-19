module Material
  (
    Player (..)
  , Board
  , Index
  , Tile (..)
  , Direction (..)
  , inBounds
  , dirToFs
  , allEmptyIs
  , allDirections
  , setBy
  , setBySame
  , left, right, up, down
  , emptyBoard
  , maybeEmpty
  , setTile
  , anyEmpty
  , allIs
  , getTile
  )
  where

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
width :: Int
width = 20

height :: Int
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

allDirections :: [Direction]
allDirections = [Horizontal .. Diagonalr]

-- Gives all indices on the board
allIs :: Board -> [Index]
allIs b = range $ bounds b

-- Checks whether the index is in bounds
inBounds :: Maybe Index -> Board -> Maybe Index
inBounds Nothing _ = Nothing
inBounds (Just i) b = if inRange (bounds b) i then Just i else Nothing

-- Checks whether the tile at the index is empty
maybeEmpty :: Index -> Board -> Maybe Index
maybeEmpty i b = if isEmpty b i then Just i else Nothing
               
-- Gives an empty board
emptyBoard :: Board
emptyBoard = array ((0,0),(h',w')) [((i,j),Empty)|j<-[0..h'],i<-[0..w']]
    where w' = width-1
          h' = height-1

-- Gives a large empty board mostly for testing parallelisation
emptyBoardL :: Board
emptyBoardL = array ((0,0),(h',w')) [((i,j),Empty)|j<-[0..h'],i<-[0..w']]
    where w' = width^2 - 1
          h' = height^2 - 1

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

-- Trys to set the tile at the specified index
setTile :: Index -> Int -> Board -> Board
setTile i p = setBoard ls
    where ls = [(i, Set p)]
    
-- Trys to get the tile at the specified index
getTile :: Index -> Board -> Maybe Tile
getTile i b = case inBounds (Just i) b of
  Just i -> Just $ b ! i
  Nothing -> Nothing

-- Gives the player
-- partial
setBy :: Board -> Index -> Player
setBy b i = Player $ playerId (b ! i)
    
-- Checks if the tile and the tile at the index are set by the
-- same player                                 
setBySame :: Maybe Index -> Tile -> Board -> Bool
setBySame Nothing  _      _ = False
setBySame _        Empty  _ = False
setBySame (Just i) t      b = b ! i == t    

-- Functions to get an adjacent index on the board
changeIndex :: (Int->Int,Int->Int) -> Index -> Index
changeIndex (f,g) (a,b) = (f a, g b)

up :: Index -> Index
up = changeIndex ((+(-1)),id)
down :: Index -> Index
down = changeIndex ((+1),id)
left :: Index -> Index
left = changeIndex (id,(+(-1)))
right :: Index -> Index
right = changeIndex (id,(+1))

-- Gives the pair of function to get adjacent indices in one direction
dirToFs :: Direction -> (Index->Index, Index->Index)
dirToFs d | d == Horizontal = (left, right)
          | d == Vertical = (up, down)
          | d == Diagonall = (up.right,down.left)
          | d == Diagonalr = (up.left,down.right)
          | otherwise = (id,id)
