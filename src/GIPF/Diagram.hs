{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module GIPF.Diagram where

import           Control.Arrow        ((***))
import           Control.Monad
import qualified Data.Map.Strict      as Map
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           GIPF.Types
import           System.IO
import           Test.QuickCheck

pieceDiagram Nothing = circle 0.01
pieceDiagram (Just (Piece Player1 NormalPiece)) = circle 0.5 # fc white
pieceDiagram (Just (Piece Player2 NormalPiece)) = circle 0.5 # fc black
pieceDiagram (Just (Piece Player1 GipfPiece)) = (circle 0.25 # fc white) `atop` (circle 0.33 # fc black) `atop` (circle 0.5 # fc white)
pieceDiagram (Just (Piece Player2 GipfPiece)) = (circle 0.2 # fc black) `atop` (circle 0.33 # fc white) `atop` (circle 0.5 # fc black)

gridDiagram (Grid g) = position pieces
  where
    pieces = map (coordToPoint *** pieceDiagram) $ Map.toList g

coordToPoint :: GIPF.Types.Point -> P2 Double
coordToPoint (q,r) =  p2 (x,y)
  where
    q' = fromIntegral q
    r' = fromIntegral r
    x = sqrt 3 * (q' + (r'/2))
    y = -3/2 * r'

test :: IO ()
test = do
  g <- generate arbitrary
  let r = concatMap (map (\c -> (coordToPoint c, circle 0.5 # lc red)). snd) $ pieceRuns g
  renderSVG "test.svg" (dims $ V2 800 800) (position r <> gridDiagram g)
  -- withFile "qctest.txt" WriteMode (\h -> do
  --     hPutStr h "["
  --     replicateM_ 30 $ do
  --       g1 <- generate arbitrary
  --       hPutStr h ("setOfPieceRun (pieceRuns $ "++show g1++") `shouldBe` setOfPieceRun "++ show (pieceRuns g1)++",")
  --     hPutStr h "]"
  --     hFlush h
  --   )
