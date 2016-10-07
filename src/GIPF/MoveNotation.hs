module GIPF.MoveNotation where

import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Validation
import           GIPF.Types

notationPointMap :: Map String Point
notationPointMap = Map.fromList $ zip (Map.elems pointNotationMap) (Map.keys pointNotationMap)

pointNotationMap :: Map Point String
pointNotationMap = Map.fromList $ concatMap (\(c,ps) -> zip  ps (map ((++) [c] . show) ([1..] :: [Int]))) $ zip ['a'..] downUpLines
  where downUpLines = map (followDir 4 UpD) [(-4,4),(-3,4),(-2,4),(-1,4),(0,4),(1,3),(2,2),(3,1),(4,0)]

notationToPoint :: String -> Either String Point
notationToPoint s =  maybe (Left $ "Piece "++ s ++" not in grid") Right $ Map.lookup s notationPointMap

pointToNotation :: Point -> Either String String
pointToNotation p = maybe (Left $ "Point "++ show p ++" not in grid") Right $ Map.lookup p pointNotationMap

pieceTypeToNotation :: PieceType -> String
pieceTypeToNotation NormalPiece = ""
pieceTypeToNotation GIPFPiece   = "G"

pieceNotation :: (PieceType, Point) -> Either String String
pieceNotation (pT,p) = (pieceTypeToNotation pT ++ ) <$> pointToNotation p

pieceMoveToNotation :: PieceMove -> AccValidation [String] String
pieceMoveToNotation (PlaceMove pM)        = mkVal $ pieceNotation pM
pieceMoveToNotation (PushMove (pT,p) dir) = makeNotation <$> mkVal (pieceNotation (pT,opositeNeighbor)) <*> mkVal (pieceNotation (NormalPiece,p))
  where
    makeNotation p1 p2 = p1++"-"++p2
    opositeNeighbor = getNeighbor p (oppositeDirection dir)
pieceMoveToNotation (RemoveMove Nothing)    = AccSuccess "x"
pieceMoveToNotation (RemoveMove (Just pcs)) = ("x" ++) . intercalate "," <$> nPcs
  where
    nPcs :: AccValidation [String] [String]
    nPcs = traverse (mkVal . pointToNotation . snd) pcs

mkVal :: Applicative f => Either a b -> AccValidation (f a) b
mkVal (Left a)  = AccFailure (pure a)
mkVal (Right b) = AccSuccess b
