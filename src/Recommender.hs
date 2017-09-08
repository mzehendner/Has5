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

{-
indexToWin :: Board -> PlayerId -> Maybe Index
indexToWin b s = case x of [] -> Nothing
                           (_:_) -> Just (head x)
    where x = testWin b s

--Checks if the game can be won in this turn
testWin :: Board -> PlayerId -> [Index]
testWin b s = map snd z
    where
        x = map (getNinD 5 b) M.allDirections<*>ix
        y = map (testWin' s) x
        z = filter ((==True).fst) (zip y ix)
        ix = filter ((==Empty).(b!)) (range (bounds b))

testWin' :: PlayerId -> ([Tile],[Tile]) -> Bool
testWin' s (ba,bb) | na + nb >= 4 = True
                   | otherwise = False
    where (na,nb) = (countSameDirect ba (Set s), countSameDirect bb (Set s))

countSameDirect :: [Tile] -> Tile -> Int
countSameDirect []      _t = 0
countSameDirect (tx:ts) t | tx == t = 1 + countSameDirect ts t
                          | otherwise = 0
-}

getLineOfTiles ::  Index -> Board -> (Index -> Index) -> [Tile]
getLineOfTiles i b f| Just ix <- M.inBounds (f i) b = (b!ix) : getLineOfTiles ix b f
                    | otherwise = []

getNinD :: Int -> Board -> Direction -> Index -> ([Tile],[Tile])
getNinD n b d i = (tk f, tk g)
    where (f,g) = M.dirToFs d
          tk = take n.getLineOfTiles i b

randI :: [(Index, Player)] -> [Player] -> Board -> IO Index
randI hs ps b = randomRIO (0, length es - 1) >>= (\n -> return $ es !! n)
    where
      es = M.allEmptyIs b

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
allValuesFor inp p = s <$> inp --`using` parList rdeepseq
    -- without: 5.320s
    -- with (map s inp) `using` parList: 4.559s
    -- a lot of sparks overflow if the list is to long:
    -- SPARKS: 320000 (11217 converted, 308612 overflowed, 0 dud, 0 GC'd, 171 fizzled)
    -- on smaller lists:
    -- SPARKS: 800 (629 converted, 0 overflowed, 0 dud, 31 GC'd, 140 fizzled)
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
