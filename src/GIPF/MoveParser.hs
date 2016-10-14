module GIPF.MoveParser where

import           Control.Applicative ((<|>))
import           GIPF.MoveNotation
import           GIPF.Types
import           Text.Trifecta

endOfLine :: Parser Char
endOfLine = newline <|> char '\r' *> newline

gipfGameParser :: Parser [(Player, PieceMove)]
gipfGameParser = zip (cycle [Player1,Player2]) <$> endBy (try pieceMoveParser) endOfLine <* eof

pieceMoveParser :: Parser PieceMove
pieceMoveParser = do
  r1 <- optional (sepEndBy1 removePieceMoveParser(char ';'))
  a <- addPieceMoveParser
  r2 <- optional (char ';' *> sepEndBy1 removePieceMoveParser (char ';'))
  skipOptional (try piecesLeftParser)
  return (r1,a,r2)

piecesLeftParser :: Parser Integer
piecesLeftParser = skipOptional spaces *> between (char '(') (char ')') decimal

pieceTypeParser :: Parser PieceType
pieceTypeParser = maybe NormalPiece (const GIPFPiece) <$> optional (char 'G')

coordParser :: Parser Point
coordParser = do
  p <- (\c n -> notationToPoint [c,n]) <$> oneOf ['a'..'i'] <*> oneOf ['1'..'9']
  case p of
    Left e -> unexpected e
    Right pt  -> return pt

pieceParser :: Parser (PieceType,Point)
pieceParser = (,) <$> pieceTypeParser <*> coordParser <* optional (char '*')

removePieceMoveParser :: Parser RemovePieceMove
removePieceMoveParser = RemoveMove <$> (char 'x' *> optional (commaSep1 pieceParser))

addPieceMoveParser :: Parser IntroducePieceMove
addPieceMoveParser = do
  p1 <- pieceParser
  mP2 <- optional (char '-' *> coordParser)
  case mP2 of
    Nothing   -> if snd p1 `elem` edge
                 then return $ PlaceMove p1
                 else unexpected "Invlaid Placement"
    (Just p2) -> maybe (unexpected "Invlaid Push") return (findDirection (snd p1) p2 >>= makeAddPieceMove p1)
    where
      makeAddPieceMove ::  (PieceType,Point) -> Direction -> Maybe IntroducePieceMove
      makeAddPieceMove (pT,p) d
        | nP `elem` edge = Just $ PushMove (pT,nP) d
        | otherwise      = Nothing
        where nP = getNeighbor p d
