import           Data.Map.Strict (Map)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           GIPF.Types
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Grid Lines" $
    it "Length" $ do
      let setOfGridLines = foldr (Set.insert . foldr Set.insert Set.empty) Set.empty gridLines
      Set.size setOfGridLines `shouldBe` 21
  describe "Place Piece" $ do
    it "Place invalid point" $
      playPiece ((4,0),Piece Player2 NormalPiece) DownD  standardGrid `shouldBe` Nothing
    it "Push off board" $
      playPiece ((3,0),Piece Player2 NormalPiece) DownD  standardGrid `shouldBe` Nothing
    it "Push None" $
      playPiece ((3,0),Piece Player1 NormalPiece) UpD  emptyGrid `shouldBe` Just placePieceOpenExpected
    it "Push 1" $
      playPiece ((3,0),Piece Player1 NormalPiece) UpD  standardGrid `shouldBe` Just placePiecePush1Expected
    it "Push 2" $
      playPiece ((3,0),Piece Player2 NormalPiece) UpD  placePiecePush1Expected `shouldBe` Just placePiecePush2Expected
    it "Push 1 Crossroads" $
      playPiece ((3,0),Piece Player1 NormalPiece) UpLeftD placePiecePush2Expected `shouldBe` Just placePiecePush1CrossExpected
  describe "Piece Runs" $ do
    it "1 Run of 4" $
      setOfPieceRun(pieceRuns (updateGrid emptyGrid pieceRun1of4)) `shouldBe` setOfPieceRun [pieceRun1of4]
    it "1 Run of 5" $
      setOfPieceRun(pieceRuns (updateGrid emptyGrid pieceRun1of5)) `shouldBe` setOfPieceRun [pieceRun1of5]
    it "2 Run of 4" $
      setOfPieceRun(pieceRuns (updateGrid emptyGrid (pieceRun1of4 ++ pieceRun2of4Part2))) `shouldBe` setOfPieceRun [pieceRun2of4Part2, pieceRun1of4]

placePieceOpenExpected :: Map (Int, Int) (Maybe Piece)
placePieceOpenExpected = updateGrid emptyGrid [((3,0), Piece Player1 NormalPiece)]

placePiecePush1Expected :: Map (Int, Int) (Maybe Piece)
placePiecePush1Expected = updateGrid standardGrid [((3,0), Piece Player1 NormalPiece),((3,-1), Piece Player2 GipfPiece)]

placePiecePush2Expected :: Map (Int, Int) (Maybe Piece)
placePiecePush2Expected = updateGrid standardGrid [((3,0), Piece Player2 NormalPiece),((3,-1), Piece Player1 NormalPiece),((3,-2), Piece Player2 GipfPiece)]

placePiecePush1CrossExpected :: Map (Int, Int) (Maybe Piece)
placePiecePush1CrossExpected = updateGrid standardGrid [((3,0), Piece Player1 NormalPiece),((2,0), Piece Player2 NormalPiece),((3,-1), Piece Player1 NormalPiece),((3,-2), Piece Player2 GipfPiece)]

pieceRun1of4 :: [((Int, Int), Piece)]
pieceRun1of4 = [((0,0), Piece Player1 NormalPiece),((1,0), Piece Player1 NormalPiece),((2,0), Piece Player1 NormalPiece),((3,0), Piece Player1 NormalPiece)]

pieceRun2of4Part2 :: [((Int, Int), Piece)]
pieceRun2of4Part2 = [((0,0), Piece Player1 NormalPiece),((0,-1), Piece Player1 NormalPiece),((0,-2), Piece Player1 NormalPiece),((0,-3), Piece Player1 NormalPiece)]

pieceRun1of5 :: [((Int, Int), Piece)]
pieceRun1of5 = [((-1,0), Piece Player1 NormalPiece),((0,0), Piece Player1 NormalPiece),((1,0), Piece Player1 NormalPiece),((2,0), Piece Player1 NormalPiece),((3,0), Piece Player1 NormalPiece)]

setOfPieceRun :: [[((Int, Int), Piece)]] -> Set (Set ((Int, Int), Piece))
setOfPieceRun = foldr (Set.insert . foldr Set.insert Set.empty) Set.empty
