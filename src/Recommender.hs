module Recommender where

import Data.Array
import Data.List (sortBy, find)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Control.Parallel.Strategies

import Material (Player, Board, Direction(..), Tile(..), Index)
import qualified Material as M

type LastSet = Index
type PlayerId = Int

-- gives the index of a random empty tile
-- would fail if all tiles are set when its called
randI :: [(Index, Player)] -> [Player] -> Board -> IO Index
randI hs ps b = randomRIO (0, length es - 1) >>= (\n -> return $ es !! n)
    where
      es = M.allEmptyIs b

getLineOfTiles ::  Index -> Board -> (Index -> Index) -> [Tile]
getLineOfTiles i b f| Just ix <- M.inBounds (f i) b = (b!ix) : getLineOfTiles ix b f
                    | otherwise = []

getNinD :: Int -> Board -> Direction -> Index -> ([Tile],[Tile])
getNinD n b d i = (tk f, tk g)
    where (f,g) = M.dirToFs d
          tk = take n.getLineOfTiles i b

-- The function used to determine a good move. But still very much beatable.
-- Just binding and choosing one of the best
recom :: [(Index, Player)] -> [Player] -> Board -> IO Index
recom mli ps b = do --(\mli' -> findBest mli' ps b) <$> return mli
  xs <- (\mli' -> findBest mli' ps b) <$> return mli
  n <- randomRIO (0, length xs - 1)
  return $ fst (xs !! n)

{-
------------------------------------
------------------------------------
Check how attractive a point is for me and for the opponent. Choose the most attractive one.
------------------------------------
-}


findBest :: [(Index, Player)] -> [Player] -> Board -> [(Index,(Integer,Bool))]
findBest mli ps b = allBest $ sortBy (\(_,(n1,_)) (_,(n2,_)) -> compare n1 n2) combinedValues
    where
      allE = M.allEmptyIs b
      combinedValues = f1 $ allValues allE b (map M.ident ps)
      allBest (x:xs)= x : takeWhile ((==snd x).snd) xs

findAnyTrue :: [(Index,(a,Bool))] -> Maybe Index
findAnyTrue xs | Just (i, _) <- find (snd.snd) xs = Just i
               | otherwise = Nothing

-- Returns a list of all values for all players.
allValues :: [Index] -> Board -> [PlayerId] -> [[(Index,(Integer,Bool))]]
allValues is b ps = allValuesFor ixs <$> ps
    where
      ixs :: [(Index,[([Tile],[Tile])])]
      ixs = zip is $ map (forDirections b) is

-- Returns a list of all values for one player
allValuesFor :: [(Index, [([Tile],[Tile])])] -> PlayerId -> [(Index,(Integer,Bool))]
allValuesFor inp p = s <$> inp `using` parBuffer 100 rdeepseq
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
      s :: Integral a => (Index, [([Tile],[Tile])]) -> (Index,(a,Bool))
      s (ix, ts) = (ix, valueTile ts p)

-- Converts the tiles in both directions with the current index set by the specified player
conv :: (Tile -> Bool) -> ([Tile],[Tile]) -> PlayerId -> [Tile]
conv f (as, bs) p = reverse (takeWhile f as) ++ Set p:takeWhile f bs

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
longestReachable ts@(as, bs) p = maximum $ longestR $ conv f ts p
    where
      f = (`elem` [Set p, Empty])
      longestR :: Integral a => [Tile] -> [a]
      longestR = foldr (\t out -> if t == Set p then (+1) (head out) : drop 1 out
                                                else 0:out
                       ) [0]

{-
( x = minimalTurns is < 5 and low is good
, y = possibleWins < 6 and high is good
, z = longestReachable < 4 and high is good)
-}
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

--Sorts by value.
sortPosByVal :: Index -> [(Index, (Int, Bool))] -> [(Index, (Int, Bool))]
sortPosByVal i = sortBy f
    where
      f (ix1, (n1,_)) (ix2,(n2,_)) = compare n1 n2

-- Computes the value of a tile for the specified Player
valueTile :: Integral a => [([Tile],[Tile])] -> PlayerId -> (a, Bool)
valueTile ts p = foldVals h1 g1 $ fmap (values p) ts
-- merge functions
h1 :: Integral a => [a] -> a
h1 (x:y:z:_) = x^3 * (6-y) * ((5-z)^3)
g1 :: Integral a => a -> a -> a
g1 = (*)
f1 :: Integral a => [[(Index, (a, Bool))]] -> [(Index, (a, Bool))]
f1 (a:b:_) = zipWith (\(i1,(n1,b1)) (_,(n2,b2)) -> (i1, (n1 * n2,b1||b2))) a b

--Function that gives an integer value determining if it's a valuable
--position or not and wether 5 can be connected at this index.
foldVals :: Integral a => ([a]->a) -> (a -> a -> a) -> [[a]] -> (a, Bool)
foldVals h g xs= (foldr (g.fst) 1 ns, b)
    where
      ns = flip valOneLine h <$> xs
      b = any snd ns
      valOneLine :: Integral a => [a] -> ([a]->a) -> (a,Bool)
      valOneLine xs h = (h xs, (==0) $ head xs)
