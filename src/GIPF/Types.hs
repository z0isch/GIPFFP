{-# LANGUAGE TemplateHaskell #-}

module GIPF.Types where

import           Control.Monad   (join)
import           Data.Bool       (bool)
import           Data.DeriveTH
import           Data.List       (nub, transpose)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, isJust, isNothing, mapMaybe)
import           Test.QuickCheck

data Player = Player1 | Player2
  deriving (Show, Eq, Ord)
derive makeArbitrary ''Player

data PieceType = NormalPiece | GipfPiece
  deriving (Show, Eq, Ord)
derive makeArbitrary ''PieceType

data Piece = Piece Player PieceType
  deriving (Show, Eq, Ord)
derive makeArbitrary ''Piece

data Direction = UpD | DownD | UpLeftD | UpRightD | DownLeftD | DownRightD
  deriving (Show, Eq, Ord, Enum)
derive makeArbitrary ''Direction

type Point = (Int,Int)

newtype Grid = Grid (Map Point (Maybe Piece))
  deriving (Show, Eq)
instance Arbitrary Grid where
  arbitrary = let (Grid g) = emptyGrid
              in Grid <$> mapM (const arbitrary) g

distance :: Point -> Point -> Int
distance (q,r) (q',r') = (abs (q - q') + abs (q + r - q' - r') + abs (r - r')) `div` 2

oppositeDirection :: Direction -> Direction
oppositeDirection UpD = DownD
oppositeDirection UpLeftD = DownRightD
oppositeDirection DownLeftD = UpRightD
oppositeDirection DownD = UpD
oppositeDirection DownRightD = UpLeftD
oppositeDirection UpRightD = DownLeftD

getDirection :: Point -> Point -> Maybe Direction
getDirection (x,y) (x',y')
  | x == x' && y == y'-1        = Just UpD
  | x == x'-1 && y == y'        = Just UpLeftD
  | x == x'-1 && y == y'+1      = Just DownLeftD
  | x == x' && y == y'+1        = Just DownD
  | x == x'+1 && y == y'        = Just DownRightD
  | x == x'+1 && y == y'-1      = Just UpRightD
  | otherwise                   = Nothing

getNeighbor :: Point -> Direction -> Point
getNeighbor (x,y) UpD = (x,y-1)
getNeighbor (x,y) UpLeftD = (x-1,y)
getNeighbor (x,y) DownLeftD = (x-1,y+1)
getNeighbor (x,y) DownD = (x,y+1)
getNeighbor (x,y) DownRightD = (x+1,y)
getNeighbor (x,y) UpRightD = (x+1,y-1)

updateGridWithPiece :: (Point, Piece) -> Grid -> Grid
updateGridWithPiece (coord,p) (Grid g) = Grid $ Map.update (const $ Just $ Just p) coord g

updateGrid :: Grid -> [(Point, Piece)] -> Grid
updateGrid = foldr updateGridWithPiece

emptyGrid :: Grid
emptyGrid = Grid $ Map.fromList [((x,y),Nothing) |x <- [-3..3], y <- [-3..3], distance (0,0) (x,y) <= 3]

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
    upDown = map (keepGoing DownD) [(-3,0),(-2,1),(-1,2),(0,-3),(1,-3),(2,-3), (3,-3)]
    oneWay = map (keepGoing DownRightD) [(0,-3),(-1,-2),(-2,-1),(-3,0),(-3,1),(-3,2),(-3,3)]
    otherWay = map (keepGoing DownLeftD) [(0,-3),(1,-3),(2,-3),(3,-3),(3,-2),(3,-1),(3,0)]
    keepGoing d (x,y)
      | distance (0,0) (x,y) <= 3 = (x,y):keepGoing d (getNeighbor (x,y) d)
      | otherwise = []

playPiece :: (Point, Piece) -> Direction -> Grid -> Maybe Grid
playPiece (c,p) d (Grid g)
  | distance (0,0) c > 3 = Nothing
  | otherwise = join finalGrid
  where
    oldPiece = Map.lookup c g
    isEmpty = isNothing <$> oldPiece
    nextGrid = updateGridWithPiece (c,p) (Grid g)
    subsequentGrid = case oldPiece of
      Just (Just op) -> playPiece (getNeighbor c d, op) d nextGrid
      _ -> Nothing
    finalGrid = bool subsequentGrid (Just nextGrid) <$> isEmpty

pieceRuns :: Grid ->  [(Player,[Point])]
pieceRuns (Grid g) = getRowExtensions (Grid g) $ map (\r -> (getPlayer $ snd $ head r, map fst r)) getRuns
  where
    getPlayer (Piece p _) = p
    getRuns = filter (not . null) $ map (concatMap fst . filter ((>= 4) . snd) . runs) mapLines
    mapLines = map (map (\a -> (a,g Map.! a))) gridLines

runs :: [(Point,Maybe Piece)] -> [([(Point,Piece)],Int)]
runs = foldr r []
  where
    r :: (Point, Maybe Piece) -> [([(Point,Piece)],Int)] -> [([(Point,Piece)],Int)]
    r (_,Nothing) xs  = ([],0):xs
    r (pt,Just p) [] = [([(pt,p)],1)]
    r (pt,Just p) (([],_):xs) = ([(pt,p)],1):xs
    r (pt,Just p) xs@((ps,c):cs)
      | playerEq p (head ps) = ((pt,p):ps,c+1):cs
      | otherwise = ([(pt,p)],1):xs
    playerEq (Piece p1 _) (_,Piece p2 _) = p1 == p2

getRowExtensions :: Grid -> [(Player,[Point])] -> [(Player,[Point])]
getRowExtensions (Grid g) = map extension
  where
    extension (p,pRun) = (p,nub $ pRun ++ keepGoing (runDir pRun) (head pRun) ++ keepGoing (oppositeDirection $ runDir pRun) (head pRun))
    runDir pRun = head $ mapMaybe (getDirection (head pRun)) pRun
    keepGoing d (x,y)
      | isJust nextPiece = (x,y):keepGoing d (fromJust nextPiece)
      | otherwise = [(x,y)]
      where
        nextPiece = const neighbor <$> join (Map.lookup neighbor g)
        neighbor = getNeighbor (x,y) d
