module GIPF.MoveParser where

import           Control.Applicative ((<|>))
import           GIPF.MoveNotation
import           GIPF.Types
import           Text.Trifecta

endOfLine :: Parser Char
endOfLine = newline <|> char '\r' *> newline

gipfGameParser :: Parser [(Player,[PieceMove])]
gipfGameParser = zip (cycle [Player1,Player2]) <$> endBy (try pieceMoveParser) endOfLine <* eof

pieceMoveParser :: Parser [PieceMove]
pieceMoveParser = semiSep1 (try addPieceMoveParser <|> removePieceMoveParser)

pieceTypeParser :: Parser PieceType
pieceTypeParser = maybe NormalPiece (const GipfPiece) <$> optional (char 'G')

coordParser :: Parser Point
coordParser = do
  p <- (\c n -> notationToPoint [c,n]) <$> oneOf ['a'..'i'] <*> oneOf ['1'..'9']
  maybe (unexpected "Invalid Point") return p

pieceParser :: Parser (PieceType,Point)
pieceParser = (,) <$> pieceTypeParser <*> coordParser <* optional (char '*')

removePieceMoveParser :: Parser PieceMove
removePieceMoveParser = RemoveMove <$> (char 'x' *> optional (commaSep1 pieceParser))

addPieceMoveParser :: Parser PieceMove
addPieceMoveParser = do
  p1 <- pieceParser
  mP2 <- optional (char '-' *> coordParser)
  case mP2 of
    Nothing   -> if snd p1 `elem` edge
                 then return $ PlaceMove p1
                 else unexpected "Invlaid Placement"
    (Just p2) -> maybe (unexpected "Invlaid Push") return (findDir (snd p1) p2 >>= makeAddPieceMove p1)
    where
      makeAddPieceMove ::  (PieceType,Point) -> Direction -> Maybe PieceMove
      makeAddPieceMove (pT,p) d
        | nP `elem` edge = Just $ PushMove (pT,nP) d
        | otherwise      = Nothing
        where nP = getNeighbor p d
