{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module GIPF.Diagram where

import           Control.Arrow               (first, (***))
import           Control.Monad
import           Data.List                   (scanl')
import qualified Data.Map.Strict             as Map
import           Data.Validation
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude
import           GIPF.MoveNotation
import           GIPF.MoveParser
import           GIPF.Types
import           Text.Trifecta               (parseFromFile)

pieceDiagram Nothing = mempty
pieceDiagram (Just (Piece Player1 NormalPiece)) = circle 0.5 # fc white
pieceDiagram (Just (Piece Player2 NormalPiece)) = circle 0.5 # fc black
pieceDiagram (Just (Piece Player1 GIPFPiece)) = (circle 0.25 # fc white) <> (circle 0.33 # fc black) <> (circle 0.5 # fc white)
pieceDiagram (Just (Piece Player2 GIPFPiece)) = (circle 0.2 # fc black) <> (circle 0.33 # fc white) <> (circle 0.5 # fc black)

pieceMoveDia Nothing = text ""
pieceMoveDia (Just m) = text note # fontSizeL 0.3
  where
    note = foldAcc $ pieceMoveToNotation m
    foldAcc (AccFailure xs) = concat xs
    foldAcc (AccSuccess x) = x

stateDiagram gs = text (show $ _gsPlayer1Pieces gs) ||| unitSpace ||| gridDiagram (_gsGrid gs) ||| unitSpace ||| text (show $ _gsPlayer2Pieces gs)

gridDiagram (Grid g) = mconcat [pieces, outerPoints, gLines, ls]
  where
    pieces = position $ map (coordToPoint *** pieceDiagram) $ Map.toList g
    outerPoints = position $ map (\e -> (coordToPoint e,circle 0.1 # fc black)) outerEdge
    gLines = mconcat (map (fromVertices . map coordToPoint) diaGridLines)
    ls = position $ map (coordToRep p2 *** (\t -> text t # fontSizeL 0.3 # bold)) gridLabels

unitSpace = unitSquare # lw 0

gridLabels :: [((Double,Double), String)]
gridLabels = [((-4.0,4.5),"a1"),((-3.0,4.5),"b1"),((-2.0,4.5),"c1"),((-1.0,4.5),"d1"),((0.0,4.5),"e1"),((1.0,3.5),"f1"),((2.0,2.5),"g1"),((3.0,1.5),"h1"),((4.0,0.5),"i1"),((-4.0,-0.5),"a5"),((-3.0,-1.5),"b6"),((-2.0,-2.5),"c7"),((-1.0,-3.5),"d8"),((0.0,-4.5),"e9"),((1.0,-4.5),"f8"),((2.0,-4.5),"g7"),((3.0,-4.5),"h6"),((4.0,-4.5),"i5")]
gridLabels' :: [((Double,Double), String)]
gridLabels' = bottom ++ top
  where
    bottom = fixSpacing 0.5 $
              zip (scanl' getNeighbor (-4,5) (concatMap (replicate 4) [DownRightD,UpRightD])) (map (:"1") ['a' ..])
    top = fixSpacing (-0.5) $
            zip (scanl' getNeighbor (-4,-1) (concatMap (replicate 4) [UpRightD,DownRightD])) (zipWith (\a b -> a : show b) ['a'..] ([5..9]++[8,7,6,5] :: [Int]))
    fixSpacing s = map (first (fromIntegral *** ((+ s) . fromIntegral)))

diaGridLines :: [[GIPF.Types.Point]]
diaGridLines = [[(-3,-1),(-3,0),(-3,1),(-3,2),(-3,3),(-3,4)],[(-2,-2),(-2,-1),(-2,0),(-2,1),(-2,2),(-2,3),(-2,4)],[(-1,-3),(-1,-2),(-1,-1),(-1,0),(-1,1),(-1,2),(-1,3),(-1,4)],[(0,-4),(0,-3),(0,-2),(0,-1),(0,0),(0,1),(0,2),(0,3),(0,4)],[(1,-4),(1,-3),(1,-2),(1,-1),(1,0),(1,1),(1,2),(1,3)],[(2,-4),(2,-3),(2,-2),(2,-1),(2,0),(2,1),(2,2)],[(3,-4),(3,-3),(3,-2),(3,-1),(3,0),(3,1)],[(-1,-3),(0,-3),(1,-3),(2,-3),(3,-3),(4,-3)],[(-2,-2),(-1,-2),(0,-2),(1,-2),(2,-2),(3,-2),(4,-2)],[(-3,-1),(-2,-1),(-1,-1),(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)],[(-4,0),(-3,0),(-2,0),(-1,0),(0,0),(1,0),(2,0),(3,0),(4,0)],[(-4,1),(-3,1),(-2,1),(-1,1),(0,1),(1,1),(2,1),(3,1)],[(-4,2),(-3,2),(-2,2),(-1,2),(0,2),(1,2),(2,2)],[(-4,3),(-3,3),(-2,3),(-1,3),(0,3),(1,3)],[(1,-4),(0,-3),(-1,-2),(-2,-1),(-3,0),(-4,1)],[(2,-4),(1,-3),(0,-2),(-1,-1),(-2,0),(-3,1),(-4,2)],[(3,-4),(2,-3),(1,-2),(0,-1),(-1,0),(-2,1),(-3,2),(-4,3)],[(4,-4),(3,-3),(2,-2),(1,-1),(0,0),(-1,1),(-2,2),(-3,3),(-4,4)],[(4,-3),(3,-2),(2,-1),(1,0),(0,1),(-1,2),(-2,3),(-3,4)],[(4,-2),(3,-1),(2,0),(1,1),(0,2),(-1,3),(-2,4)],[(4,-1),(3,0),(2,1),(1,2),(0,3),(-1,4)]]
diaGridLines' :: [[GIPF.Types.Point]]
diaGridLines' = diaUpDownLine ++ diaDownRightLine ++ diaDownLeftLine
  where
    diaUpDownLine = map (followDir 4 DownD) [(-3,-1),(-2,-2),(-1,-3),(0,-4),(1,-4),(2,-4), (3,-4)]
    diaDownRightLine = map (followDir 4 DownRightD) [(-1,-3),(-2,-2),(-3,-1),(-4,0),(-4,1),(-4,2),(-4,3)]
    diaDownLeftLine = map (followDir 4 DownLeftD) [(1,-4),(2,-4),(3,-4),(4,-4),(4,-3),(4,-2),(4,-1)]


coordToRep :: ((Double, Double) -> a)  -> (Double,Double) -> a
coordToRep f (q,r) = f (x,y)
  where
    q' = q
    r' = r
    y = -sqrt 3 * (r' + (q'/2))
    x = 3/2 * q'

coordToPoint :: GIPF.Types.Point -> P2 Double
coordToPoint = coordToRep p2 . (fromIntegral *** fromIntegral)

testGame :: IO ()
testGame = do
  mMoves <- parseFromFile gipfGameParser "testGame.gipf"
  case mMoves of
    Nothing  -> putStrLn "Can't parse file"
    Just moves ->   let
                        game =  scanl' (\(g,_) (pl,m) -> (join $ flip (makeMove pl) m <$> g, Just m)) (Right emptyGrid,Nothing) moves
                        gameDiagrams = zip ([1..] :: [Int]) $ map (first (fmap gridDiagram)) game
                    in mapM_ (\(i,(d,m)) -> case d of
                                Left err -> putStrLn err
                                Right gD  -> renderRasterific ("generatedImages/testGame"++show i++".png") (dims $ V2 800 800) (unitSpace === pieceMoveDia m === unitSpace === gD === unitSpace)) gameDiagrams
