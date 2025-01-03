import Control.Monad
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (isPrefixOf, nub, partition, intercalate)
import Data.Map (Map)
import qualified Data.Map as M

type Vertex = String
type Edge = Set Vertex
type Clique = Set Vertex

main1, main2 :: IO ()
main1 = readFile "./input.txt" >>= print . solve1. map toInput . lines
main2 = readFile "./input.txt" >>= print . solve2 . map toInput . lines

toInput :: String -> Edge
toInput s = let (a, b) = span (/='-') s
            in S.fromList [a, tail b]

solve1 :: [Edge] -> Int
solve1 es = let ns = allNeighbours es
                clique = computeCliqueN ns S.empty (M.keys ns) 3
            in length $ filter (any (isPrefixOf "t")) $ map S.toList clique

allNeighbours :: [Edge] -> M.Map Vertex (Set Vertex)
allNeighbours = M.unionsWith S.union . map toMap
 where
  toMap :: Edge -> Map Vertex (Set Vertex)
  toMap e = M.fromList [ (v, S.delete v e) | v <- S.toList e ]

connectedToAll :: Map Vertex (Set Vertex) -> Vertex -> Set Vertex -> Bool
connectedToAll ns v vs = maybe False (vs `S.isSubsetOf`) (ns M.!? v)

computeCliqueN :: MonadPlus m => Map Vertex (Set Vertex) -> Clique -> [Vertex] -> Int -> m Clique
computeCliqueN  _ c  _ 0 = return c
computeCliqueN _ _ [] _ = mzero
computeCliqueN ns currCliq (v : vs) n
  | not (connectedToAll ns v currCliq) = computeCliqueN ns currCliq vs n
  | otherwise = computeCliqueN ns (S.insert v currCliq) vs (n - 1)
        `mplus` computeCliqueN ns currCliq vs n

solve2 :: [Edge] -> [Char]
solve2 es = let ns = allNeighbours es
                (ts, nts) = partition (isPrefixOf "t") (M.keys ns)
                cliques = map (computeCliqueN ns S.empty (ts ++ nts)) [1..] :: [Maybe Clique]
            in intercalate "," $ S.toAscList $ select cliques

select :: [Maybe Clique] -> Clique
select (x : y : xs) = case y of
  Nothing -> fromJust x
  Just c  | not (any (isPrefixOf "t") (S.toList c)) -> c
          | otherwise                               -> select (y : xs)