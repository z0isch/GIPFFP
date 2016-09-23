module GIPF.Types where

import           Control.Monad   (join)
import           Data.Bool       (bool)
import           Data.List       (transpose)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, isNothing)

data Player = Player1 | Player2
  deriving (Show, Eq, Ord)

data PieceType = NormalPiece | GipfPiece
  deriving (Show, Eq, Ord)

data Piece = Piece Player PieceType
  deriving (Show, Eq, Ord)

data Direction = UpD | DownD | UpLeftD | UpRightD | DownLeftD | DownRightD
  deriving (Show, Eq, Ord, Enum)

type Point = (Int,Int)
type Grid = Map Point (Maybe Piece)

distance :: Point -> Point -> Int
distance (q,r) (q',r') = (abs (q - q') + abs (q + r - q' - r') + abs (r - r')) `div` 2

getNeighbor :: Point -> Direction -> Point
getNeighbor (x,y) UpD = (x,y-1)
getNeighbor (x,y) UpLeftD = (x-1,y)
getNeighbor (x,y) DownLeftD = (x-1,y+1)
getNeighbor (x,y) DownD = (x,y+1)
getNeighbor (x,y) DownRightD = (x+1,y)
getNeighbor (x,y) UpRightD = (x+1,y-1)

updateGridWithPiece :: (Point, Piece) -> Grid -> Grid
updateGridWithPiece (coord,p) = Map.update (const $ Just $ Just p) coord

updateGrid :: Grid -> [(Point, Piece)] -> Grid
updateGrid = foldr updateGridWithPiece

emptyGrid :: Grid
emptyGrid = Map.fromList [((x,y),Nothing) |x <- [-3..3], y <- [-3..3], distance (0,0) (x,y) <= 3]

standardGrid :: Grid
standardGrid = updateGrid emptyGrid (zip corners players)
  where
    players = concat $ transpose [replicate 3 (Piece Player2 GipfPiece), replicate 3 (Piece Player1 GipfPiece)]

edge :: [Point]
edge = [(0,-3),(1,-3),(2,-3),(3,-3),(3,-2),(3,-1),(3,0),(2,1),(1,2),(0,3),(-1,3),(-2,3),(-3,3),(-3,2),(-3,1),(-3,0),(-2,-1),(-1,-2)]

corners :: [Point]
corners = [(0,-3), (3,-3), (3,0), (0,3), (-3,3), (-3,0)]

gridLines :: [[Point]]
gridLines = [[(-3,0),(-3,1),(-3,2),(-3,3)],[(-2,1),(-2,2),(-2,3)],[(-1,2),(-1,3)],[(0,-3),(0,-2),(0,-1),(0,0),(0,1),(0,2),(0,3)],[(1,-3),(1,-2),(1,-1),(1,0),(1,1),(1,2)],[(2,-3),(2,-2),(2,-1),(2,0),(2,1)],[(3,-3),(3,-2),(3,-1),(3,0)],[(0,-3),(1,-3),(2,-3),(3,-3)],[(-1,-2),(0,-2),(1,-2),(2,-2),(3,-2)],[(-2,-1),(-1,-1),(0,-1),(1,-1),(2,-1),(3,-1)],[(-3,0),(-2,0),(-1,0),(0,0),(1,0),(2,0),(3,0)],[(-3,1),(-2,1),(-1,1),(0,1),(1,1),(2,1)],[(-3,2),(-2,2),(-1,2),(0,2),(1,2)],[(-3,3),(-2,3),(-1,3),(0,3)],[(0,-3),(-1,-2),(-2,-1),(-3,0)],[(1,-3),(0,-2),(-1,-1),(-2,0),(-3,1)],[(2,-3),(1,-2),(0,-1),(-1,0),(-2,1),(-3,2)],[(3,-3),(2,-2),(1,-1),(0,0),(-1,1),(-2,2),(-3,3)],[(3,-2),(2,-1),(1,0),(0,1),(-1,2),(-2,3)],[(3,-1),(2,0),(1,1),(0,2),(-1,3)],[(3,0),(2,1),(1,2),(0,3)]]
gridLines' :: [[Point]]
gridLines' = upDown ++ oneWay ++ otherWay
  where
    upDown = map (`keepGoing` DownD) [(-3,0),(-2,1),(-1,2),(0,-3),(1,-3),(2,-3), (3,-3)]
    oneWay = map (`keepGoing` DownRightD) [(0,-3),(-1,-2),(-2,-1),(-3,0),(-3,1),(-3,2),(-3,3)]
    otherWay = map (`keepGoing` DownLeftD) [(0,-3),(1,-3),(2,-3),(3,-3),(3,-2),(3,-1),(3,0)]
    keepGoing :: Point -> Direction -> [Point]
    keepGoing (x,y) d
      | distance (0,0) (x,y) <= 3 = (x,y):keepGoing (getNeighbor (x,y) d) d
      | otherwise = []

playPiece :: (Point, Piece) -> Direction -> Grid -> Maybe Grid
playPiece = go
  where
    go (c,p) d g
      | distance (0,0) c > 3 = Nothing
      | otherwise = join finalGrid
      where
        oldPiece = Map.lookup c g
        isEmpty = isNothing <$> oldPiece
        nextGrid = updateGridWithPiece (c,p) g
        subsequentGrid = case oldPiece of
          Just (Just op) -> go (getNeighbor c d, op) d nextGrid
          _ -> Nothing
        finalGrid = bool subsequentGrid (Just nextGrid) <$> isEmpty

runs :: [(Point,Maybe Piece)] -> [([(Point,Piece)],Int)]
runs = foldr r []
  where
    r :: (Point, Maybe Piece) -> [([(Point,Piece)],Int)] -> [([(Point,Piece)],Int)]
    r (_,Nothing) xs = xs
    r a [] = [([fromJust <$> a],1)]
    r a t@((x:xs,c):cs)
      | playerEq a x = ((fromJust <$> a):x:xs,c+1):cs
      | otherwise = ([fromJust <$> a],1):t
    playerEq :: (Point,Maybe Piece) -> (Point,Piece) -> Bool
    playerEq (_,Nothing) _ = False
    playerEq (_,Just (Piece p1 _)) (_,Piece p2 _) = p1 == p2


pieceRuns :: Grid -> [[(Point, Piece)]]
pieceRuns g = map (concatMap fst) getRuns
  where
    getRuns = filter (not . null) $ map (filter ((>= 4) . snd) . runs) mapLines
    mapLines = map (map (\a -> (a,g Map.! a))) gridLines
