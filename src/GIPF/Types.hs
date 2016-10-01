{-# LANGUAGE TemplateHaskell #-}

module GIPF.Types where

import           Control.Monad   (join)
import           Data.Bool       (bool)
import           Data.DeriveTH   (derive, makeArbitrary)
import           Data.List       (find, transpose)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, isJust, isNothing, mapMaybe)
import qualified Data.Set        as Set
import           Safe            (headMay)
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

data PieceMove = PlaceMove (PieceType,Point) | PushMove (PieceType,Point) Direction | RemoveMove (Maybe [(PieceType,Point)])
  deriving (Eq,Ord,Show)

distance :: Point -> Point -> Int
distance (q,r) (q',r') = (abs (q - q') + abs (q + r - q' - r') + abs (r - r')) `div` 2

oppositeDirection :: Direction -> Direction
oppositeDirection UpD        = DownD
oppositeDirection UpLeftD    = DownRightD
oppositeDirection DownLeftD  = UpRightD
oppositeDirection DownD      = UpD
oppositeDirection DownRightD = UpLeftD
oppositeDirection UpRightD   = DownLeftD

getDirection :: Point -> Point -> Maybe Direction
getDirection (x,y) (x',y')
  | x == x' && y == y'-1   = Just UpD
  | x == x'-1 && y == y'   = Just UpLeftD
  | x == x'-1 && y == y'+1 = Just DownLeftD
  | x == x' && y == y'+1   = Just DownD
  | x == x'+1 && y == y'   = Just DownRightD
  | x == x'+1 && y == y'-1 = Just UpRightD
  | otherwise              = Nothing

findDir :: Point -> Point -> Maybe Direction
findDir p1 p2 = fmap fst mDir
  where mDir = find (elem p2 . snd) $ map (\d -> (d,followDir 4 d p1)) [UpD ..]

getNeighbor :: Point -> Direction -> Point
getNeighbor (x,y) UpD        = (x,y-1)
getNeighbor (x,y) UpLeftD    = (x-1,y)
getNeighbor (x,y) DownLeftD  = (x-1,y+1)
getNeighbor (x,y) DownD      = (x,y+1)
getNeighbor (x,y) DownRightD = (x+1,y)
getNeighbor (x,y) UpRightD   = (x+1,y-1)

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
gridLines = [[(-3,0),(-3,1),(-3,2),(-3,3)],[(-2,-1),(-2,0),(-2,1),(-2,2),(-2,3)],[(-1,-2),(-1,-1),(-1,0),(-1,1),(-1,2),(-1,3)],[(0,-3),(0,-2),(0,-1),(0,0),(0,1),(0,2),(0,3)],[(1,-3),(1,-2),(1,-1),(1,0),(1,1),(1,2)],[(2,-3),(2,-2),(2,-1),(2,0),(2,1)],[(3,-3),(3,-2),(3,-1),(3,0)],[(0,-3),(1,-3),(2,-3),(3,-3)],[(-1,-2),(0,-2),(1,-2),(2,-2),(3,-2)],[(-2,-1),(-1,-1),(0,-1),(1,-1),(2,-1),(3,-1)],[(-3,0),(-2,0),(-1,0),(0,0),(1,0),(2,0),(3,0)],[(-3,1),(-2,1),(-1,1),(0,1),(1,1),(2,1)],[(-3,2),(-2,2),(-1,2),(0,2),(1,2)],[(-3,3),(-2,3),(-1,3),(0,3)],[(0,-3),(-1,-2),(-2,-1),(-3,0)],[(1,-3),(0,-2),(-1,-1),(-2,0),(-3,1)],[(2,-3),(1,-2),(0,-1),(-1,0),(-2,1),(-3,2)],[(3,-3),(2,-2),(1,-1),(0,0),(-1,1),(-2,2),(-3,3)],[(3,-2),(2,-1),(1,0),(0,1),(-1,2),(-2,3)],[(3,-1),(2,0),(1,1),(0,2),(-1,3)],[(3,0),(2,1),(1,2),(0,3)]]
gridLines' :: [[Point]]
gridLines' = upDownLine ++ downRightLine ++ downLeftLine

upDownLine :: [[(Int, Int)]]
upDownLine = map (followDir 3 DownD) [(-3,0),(-2,-1),(-1,-2),(0,-3),(1,-3),(2,-3), (3,-3)]
downRightLine :: [[(Int, Int)]]
downRightLine = map (followDir 3 DownRightD) [(0,-3),(-1,-2),(-2,-1),(-3,0),(-3,1),(-3,2),(-3,3)]
downLeftLine :: [[(Int, Int)]]
downLeftLine = map (followDir 3 DownLeftD) [(0,-3),(1,-3),(2,-3),(3,-3),(3,-2),(3,-1),(3,0)]

followDir :: Int -> Direction -> (Int, Int) -> [(Int, Int)]
followDir i d (x,y)
  | distance (0,0) (x,y) <= i = (x,y):followDir i d (getNeighbor (x,y) d)
  | otherwise                 = []

playPiece :: (Point, Piece) -> Direction -> Grid -> Maybe Grid
playPiece (c,p) d (Grid g)
  | distance (0,0) c > 3 = Nothing
  | otherwise            = join finalGrid
  where
    oldPiece = Map.lookup c g
    isEmpty = isNothing <$> oldPiece
    nextGrid = updateGridWithPiece (c,p) (Grid g)
    subsequentGrid = case oldPiece of
      Just (Just op) -> playPiece (getNeighbor c d, op) d nextGrid
      _ -> Nothing
    finalGrid = bool subsequentGrid (Just nextGrid) <$> isEmpty

pieceRuns :: Grid -> [(Player,[Point])]
pieceRuns (Grid g) = getRowExtensions (Grid g) playerRuns
  where
    playerRuns = map (\r -> (getPlayer $ snd $ head r, map fst r)) getRuns
    getPlayer (Piece p _) = p
    getRuns = filter (not . null) $ map (concatMap fst . filter ((>= 4) . snd) . runs) mapLines
    mapLines = map (map (\a -> (a,g Map.! a))) gridLines

runs :: [(Point,Maybe Piece)] -> [([(Point,Piece)],Int)]
runs = foldr r []
  where
    r :: (Point, Maybe Piece) -> [([(Point,Piece)],Int)] -> [([(Point,Piece)],Int)]
    r (_,Nothing) xs          = ([],0):xs
    r (pt,Just p) []          = [([(pt,p)],1)]
    r (pt,Just p) (([],_):xs) = ([(pt,p)],1):xs
    r (pt,Just p) xs@((ps,c):cs)
      | playerEq p (head ps) = ((pt,p):ps,c+1):cs
      | otherwise            = ([(pt,p)],1):xs
    playerEq (Piece p1 _) (_,Piece p2 _) = p1 == p2

getRowExtensions :: Grid -> [(Player,[Point])] -> [(Player,[Point])]
getRowExtensions (Grid g) = map extension
  where
    extension (pl,[])    = (pl,[])
    extension (pl,[p])   = (pl,[p])
    extension (pl, p:ps) = (pl, nub' pieces)
      where
        nub' = Set.toList . Set.fromList
        pieces = (p:ps) ++ oneWay ++ otherWay
        rDir = headMay $ mapMaybe (getDirection p) ps
        oneWay = maybe [] (keepGoing p) rDir
        otherWay = maybe [] (keepGoing p . oppositeDirection) rDir
    keepGoing (x,y) d
      | isJust nextPiece = (x,y):keepGoing (fromJust nextPiece) d
      | otherwise        = [(x,y)]
      where
        nextPiece = const neighbor <$> join (Map.lookup neighbor g)
        neighbor = getNeighbor (x,y) d

makeMove :: Player -> Grid -> PieceMove -> Either String Grid
makeMove pl (Grid g) (PlaceMove (pT,p)) = case Map.lookup p g of
  Just _  -> Right $ Grid $ Map.insert p (Just $ Piece pl pT) g
  Nothing -> Left "Can't place a piece on top of another piece"
makeMove pl g (PushMove (pT,p) d) = maybe (Left "Invalid push") Right $ playPiece (p, Piece pl pT) d g
makeMove _ (Grid g) (RemoveMove (Just rs))
  | pcsExist = Right $ Grid $ foldr (Map.adjust (const Nothing) . snd) g rs
  | otherwise = Left "All the pieces do not exist in the grid"
  where
    pcsExist = isJust $ traverse (\(pT,r) -> r `Map.lookup` g >>= fmap (pieceTypeEq pT)) rs
    pieceTypeEq t (Piece _ t') = t == t'
makeMove _ _ (RemoveMove Nothing) = undefined
