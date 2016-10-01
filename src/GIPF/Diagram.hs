{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module GIPF.Diagram where

import           Control.Arrow               ((***))
import           Control.Monad
import           Data.List                   (foldl', scanl')
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromJust)
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude
import           GIPF.MoveParser
import           GIPF.Types
import           System.IO
import           Test.QuickCheck             hiding ((===))
import           Text.Trifecta               (parseFromFile)

pieceDiagram Nothing = circle 0.01
pieceDiagram (Just (Piece Player1 NormalPiece)) = circle 0.5 # fc white
pieceDiagram (Just (Piece Player2 NormalPiece)) = circle 0.5 # fc black
pieceDiagram (Just (Piece Player1 GipfPiece)) = (circle 0.25 # fc white) <> (circle 0.33 # fc black) <> (circle 0.5 # fc white)
pieceDiagram (Just (Piece Player2 GipfPiece)) = (circle 0.2 # fc black) <> (circle 0.33 # fc white) <> (circle 0.5 # fc black)

gridDiagram (Grid g) = position pieces
  where
    pieces = map (coordToPoint *** pieceDiagram) $ Map.toList g

coordToPoint :: GIPF.Types.Point -> P2 Double
coordToPoint (q,r) =  p2 (x,y)
  where
    q' = fromIntegral q
    r' = fromIntegral r
    y = -sqrt 3 * (r' + (q'/2))
    x = 3/2 * q'

moveText = text "test" # fontSizeL 0.2
unitSpace = unitSquare # lw 0

testGame :: IO ()
testGame = do
  mMoves <- parseFromFile gipfGameParser "testGame.gipf"
  case mMoves of
    Nothing  -> putStrLn "Can't parse file"
    Just moves ->   let flatMoves :: [(Player,PieceMove)]
                        flatMoves = concatMap (\(pl, mvs) -> map (\m -> (pl,m)) mvs) moves
                        game =  scanl' (\g (pl,m) -> join $ makeMove pl <$> g <*> pure m) (pure emptyGrid) flatMoves
                        gameDiagrams = zip [1..] $ map (fmap gridDiagram) game
                    in mapM_ (\(i,d) -> case d of
                                Left err -> putStrLn err
                                Right gD  -> renderRasterific ("generatedImages/testGame"++show i++".png") (dims $ V2 800 800) (unitSpace === gD === unitSpace)) gameDiagrams

test :: IO ()
test = do
  renderRasterific "test.png" (dims $ V2 800 800) (gridDiagram (updateGrid emptyGrid [((0,3), Piece Player1 NormalPiece)]))
  -- g <- generate arbitrary
  -- let r = concatMap (map (\c -> (coordToPoint c, circle 0.5 # lc red)). snd) $ pieceRuns g
  -- renderRasterific "test.png" (dims $ V2 800 800) (position r <> gridDiagram g)
  -- withFile "qctest.txt" WriteMode (\h -> do
  --     hPutStr h "["
  --     replicateM_ 30 $ do
  --       g1 <- generate arbitrary
  --       hPutStr h ("setOfPieceRun (pieceRuns $ "++show g1++") `shouldBe` setOfPieceRun "++ show (pieceRuns g1)++",")
  --     hPutStr h "]"
  --     hFlush h
  --   )
