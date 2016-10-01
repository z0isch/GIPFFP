module GIPF.MoveNotation where

import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GIPF.Types

notationPointMap :: Map String Point
notationPointMap = Map.fromList $ zip (Map.elems pointNotationMap) (Map.keys pointNotationMap)

pointNotationMap :: Map Point String
pointNotationMap = Map.fromList $ concatMap (\(c,ps) -> zip  ps (map ((++) [c] . show) ([1..] :: [Int]))) $ zip ['a'..] downUpLines
  where downUpLines = map (followDir 4 UpD) [(-4,4),(-3,4),(-2,4),(-1,4),(0,4),(1,3),(2,2),(3,1),(4,0)]

notationToPoint :: String -> Maybe Point
notationToPoint s = Map.lookup s notationPointMap

pointToNotation :: Point -> Maybe String
pointToNotation p = Map.lookup p pointNotationMap

pieceTypeToNotation :: PieceType -> String
pieceTypeToNotation NormalPiece = ""
pieceTypeToNotation GipfPiece = "G"

pieceNotation :: (PieceType, Point) -> Maybe String
pieceNotation (pT,p) = (++ pieceTypeToNotation pT) <$> pointToNotation p

pieceMoveToNotation :: PieceMove -> Maybe String
pieceMoveToNotation (PlaceMove pM) = pieceNotation pM
pieceMoveToNotation (PushMove (pT,p) dir) = makeNotation <$> pieceNotation (pT,p) <*> neighborNotation
  where
    makeNotation p1 p2 = p1++"-"++p2
    neighborNotation = pointToNotation (getNeighbor p dir)
pieceMoveToNotation (RemoveMove Nothing) = Just "x"
pieceMoveToNotation (RemoveMove (Just pcs)) = intercalate "," <$> traverse (pointToNotation . snd) pcs
