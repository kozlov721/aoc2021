module Day15 where

import Data.Matrix (Matrix)
import Day2 (Position)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.Map (Map)
import Data.Maybe (isJust)
import Day9 (neighbours4, readIntMatrix)

import qualified Data.Matrix as M
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as Map


aStar :: Matrix Int -> ([Position], Int)
aStar m = go m
    (M.nrows m, M.ncols m)
    (Q.singleton 0 (1, 1))
    Map.empty
    (Map.singleton (1, 1) 0)
  where
    go :: Matrix Int             -- ^ Matrix of weights
       -> Position               -- ^ End
       -> MinPQueue Int Position -- ^ Visited (f-score, node)
       -> Map Position Position  -- ^ Predecessors
       -> Map Position Int       -- ^ g-scores
       -> ([Position], Int)      -- ^ (best path, total cost)
    go m end v pred gs
        | null v         = error "No path exists"
        | current == end = (path, gs Map.! end)
        | otherwise      = go m end newVisited newPred newGs
      where
        ((f, current), visited) = Q.deleteFindMin v
        betterNodes = filter
                (  \(c, tg)
                -> tg < Map.findWithDefault (maxBound :: Int) c gs
                )
            $ map
                (  \c@(x, y)
                -> (c, (gs Map.! current) + (m M.! (y, x)))
                )
            adj
        (newVisited, newPred, newGs) = foldl
            updateTables
            (visited, pred, gs)
            betterNodes
        updateTables (v, p, g) (c@(x, y), tg) =
            ( Q.insert (tg + h x y) c v
            , Map.insert c current p
            , Map.insert c tg g
            )
        adj = filter (isJust . (flip . uncurry) M.safeGet m)
            $ uncurry neighbours4 current
        danger = m M.! current
        h x y = (M.nrows m - y) + (M.ncols m - x)
        path = reverse $ reconstructPath end
        reconstructPath node
            | not $ Map.member node pred = [node]
            | otherwise = node : reconstructPath (pred Map.! node)

enlargeCaveMap :: Matrix Int -> Matrix Int
enlargeCaveMap m = foldl (M.<->) extendedRight
    $ map (increaseRisk extendedRight) [1..4]
  where
    extendedRight = foldl (M.<|>) m
        $ map (increaseRisk m) [1..4]
    increaseRisk m n = (\x -> (+1) $ (x + n - 1) `mod` 9) <$> m

day15 :: String -> IO ()
day15 str = do
    let smallMap = readIntMatrix str
    putStr "Total risk of the best path in the small map: "
    print $ snd $ aStar smallMap
    let largeMap = enlargeCaveMap smallMap
    putStr "Total risk of the best path in the large map: "
    print $ snd $ aStar largeMap


