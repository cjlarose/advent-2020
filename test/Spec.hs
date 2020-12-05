import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>), (<.>), takeBaseName)
import Data.List (sort)
import Data.Binary.Put (runPut, putCharUtf8, putStringUtf8)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)

import Advent2020.Solve (solverForProblem)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

resultToBS :: Either String PuzzleAnswerPair -> LBS.ByteString
resultToBS (Left err) = runPut $ putStringUtf8 err >> putCharUtf8 '\n'
resultToBS (Right (PuzzleAnswerPair (part1, part2))) = runPut $ do
                                                         putStringUtf8 part1
                                                         putCharUtf8 '\n'
                                                         putStringUtf8 part2
                                                         putCharUtf8 '\n'

main :: IO ()
main = defaultMain =<< goldenTests

goldenTestForProblem :: Int -> TestTree
goldenTestForProblem n = goldenVsString testName outputFile action
  where
    testName = "Day " ++ show n
    outputFile = "outputs" </> show n <.> "txt"
    action = resultToBS <$> solverForProblem n

goldenTests :: IO TestTree
goldenTests = do
  inputFiles <- findByExtension [".txt"] "inputs"
  let problemNumbers = sort . map (read . takeBaseName) $ inputFiles
  return . testGroup "Golden Tests" $ map goldenTestForProblem problemNumbers
