module Recommender
  (
    random
  , recom
  )
  where

import Data.Array
import Data.List (sortBy, find)
import System.Random (randomRIO)
import Control.Parallel.Strategies
import Control.Concurrent.STM
import System.IO.Unsafe

import Material (Player, Board, Direction(..), Tile(..), Index)
import qualified Material as M

type PlayerId = Int


-- Returns a line of tiles consisting of all tiles at the indices created by the function.
getLineOfTiles ::  Index -> Board -> (Index -> Index) -> [Tile]
getLineOfTiles i b f| Just ix <- M.inBounds (Just $ f i) b = (b!ix) : getLineOfTiles ix b f
                    | otherwise = []

-- Gets n-tiles "left" and "right" of the index in the given direction
getNinD :: Int -> Board -> Direction -> Index -> ([Tile],[Tile])
getNinD n b d i = (tk f, tk g)
    where (f,g) = M.dirToFs d
          tk = take n.getLineOfTiles i b

-- gives the index of a random empty tile
-- would fail if all tiles are set when its called
-- TODO Remove need for unsafePerformIO
random :: Index -> [(Index,Player)] -> [Player] -> Board -> STM Index
random _ _ _ b = return $ unsafePerformIO $ randomRIO (0, length es - 1) >>= (\n -> return $ es !! n)
    where
      es = M.allEmptyIs b

-- TODO Remove need for unsafePerformIO
recom :: Index -> [(Index,Player)] -> [Player] -> Board -> STM Index
recom _ mli ps b = do
  xs <- (\mli' -> findBest mli' ps b) <$> return mli
  n <- return $! unsafePerformIO $ randomRIO (0, length xs - 1)
  return $ fst (xs !! n)

-- Returns the best indices and their computed values
findBest :: [(Index, Player)] -> [Player] -> Board -> [(Index,(Integer,Bool))]
findBest _ ps b = allBest $ sortBy (\(_,(n1,_)) (_,(n2,_)) -> compare n1 n2) combinedValues
    where
      allE = M.allEmptyIs b
       -- combines the values for all players
      combinedValues = f1 $ allValues allE b (map M.ident ps)
      allBest (x:xs)= x : takeWhile ((==snd x).snd) xs
      allBest [] = []

-- Returns a list of lists of values for each player
allValues :: [Index] -> Board -> [PlayerId] -> [[(Index,(Integer,Bool))]]
allValues is b ps = allValuesFor ixs <$> ps
    where
      -- The list of indices mapped with the lines of adjacent tiles in all directions
      ixs :: [(Index,[([Tile],[Tile])])]
      ixs = zip is $ map (forDirections b) is

-- Returns a list of all values for one player
allValuesFor :: [(Index, [([Tile],[Tile])])] -> PlayerId -> [(Index,(Integer,Bool))]
allValuesFor inp p = vals <$> inp `using` parBuffer 100 rdeepseq
    -- Tested on a longer list than during runtime to show the differences
    -- Tested with: +RTS -N4 -s
    -- `using` parBuffer 100 rdeepseq
    -- no more overflowing sparks and a signifiant speed up and less memory usage
    -- SPARKS: 320000 (319969 converted, 0 overflowed, 0 dud, 9 GC'd, 22 fizzled)
    -- with: s <$> inp `using` parBuffer 100 rdeepseq: 1.857s
    -- `using` parList rdeepseq
    -- without: 5.320s
    -- with (map s inp) `using` parList: 4.559s
    -- a lot of sparks overflow if the list is to long:
    -- SPARKS: 320000 (11217 converted, 308612 overflowed, 0 dud, 0 GC'd, 171 fizzled)
    -- On smaller lists there is nearly no difference in speed or memory usage between
    -- these two options.
    where
      vals :: Integral a => (Index, [([Tile],[Tile])]) -> (Index,(a,Bool))
      vals (ix, ts) = (ix, valueTile ts p)

-- Converts the tiles in both directions with the current index set by the specified player
conv :: (Tile -> Bool) -> ([Tile],[Tile]) -> PlayerId -> [Tile]
conv f (as, bs) p = reverse (takeWhile f as) ++ Set p:takeWhile f bs


-- ( x = minimalTurns is < 5 and low is good
-- , y = possibleWins < 6 and high is good
-- , z = longestReachable < 4 and high is good)
--Counts how many possible lines of 5 are in this row
possibleWins :: Integral a => [Tile] -> PlayerId -> a
possibleWins = possibleWins' 0
    where
      possibleWins' :: Integral a => a -> [Tile] -> PlayerId -> a
      possibleWins' _ []     _ = 0
      possibleWins' n (x:xs) p | x `elem` [Set p, Empty] = possibleWins' (n+1) xs p
                                             + (if n>=4 then 1 else 0)
                               | otherwise = possibleWins' 0 xs p

--Counts the minimal number of turns to victory in this row
minimalTurns :: Integral a => [Tile] -> PlayerId -> a
minimalTurns ts p = minimum $ minimalTurns' ts
    where
      minimalTurns' :: Integral a => [Tile] -> [a]
      minimalTurns' [] = [5]
      minimalTurns' ts = counts (take 5 ts):minimalTurns' (drop 1 ts)
      counts xs | length xs < 5 = 5
                | otherwise = counts' xs 0
      counts' []     n = n
      counts' (t:ts) n | t `notElem` [Set p, Empty] = 5
                       | otherwise = counts' ts (if t == Empty then n+1 else n)

--Gives the biggest connected line that can be reached
longestReachable :: Integral a => ([Tile],[Tile]) -> PlayerId -> a
longestReachable ts p = maximum $ longestR $ conv f ts p
    where
      f = (`elem` [Set p, Empty])
      longestR :: Integral a => [Tile] -> [a]
      longestR = foldr (\t out -> if t == Set p then (+1) (head out) : drop 1 out
                                                else 0:out
                       ) [0]


-- Gives a List of values
-- Order of functions is [minimalTurns, possibleWins, longestReachable]
values :: Integral a => PlayerId -> ([Tile],[Tile]) -> [a]
values p ts = [minimalTurns ts2, possibleWins ts2, longestReachable ts]<*>[p]
    where ts2 = conv (const True) ts p

--Gives a list of the surrounding lines for the index
forDirections :: Board -> Index -> [([Tile],[Tile])]
forDirections b i = fs <*> [i]
    where
      fs = map (getNinD 4 b) M.allDirections

--Function that gives an integer value determining if it's a valuable
--position or not and whether 5 can be connected at this index.
foldVals :: Integral a => ([a]->a) -> (a -> a -> a) -> [[a]] -> (a, Bool)
foldVals h g xs= (foldr (g.fst) 1 ns, b)
    where
      ns = flip valOneLine h <$> xs
      b = any snd ns
      valOneLine :: Integral a => [a] -> ([a]->a) -> (a,Bool)
      valOneLine xs h = (h xs, (==0) $ head xs)

-- Computes the value of a tile for the specified Player
valueTile :: Integral a => [([Tile],[Tile])] -> PlayerId -> (a, Bool)
valueTile ts p = foldVals h1 g1 $ fmap (values p) ts
-- merge functions
-- merges the 3 values in one direction
h1 :: Integral a => [a] -> a
h1 (x:y:z:_) = x^3 * (6-y) * ((5-z)^3)
-- defines how the values of on index are to be merged
g1 :: Integral a => a -> a -> a
g1 = (*)
-- defines how the list of values for the players are to be merged
-- in this case only the first two players
f1 :: Integral a => [[(Index, (a, Bool))]] -> [(Index, (a, Bool))]
f1 (a:b:_) = zipWith (\(i1,(n1,b1)) (_,(n2,b2)) -> (i1, (n1 * n2,b1||b2))) a b