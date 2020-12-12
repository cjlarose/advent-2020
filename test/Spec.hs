import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>), (<.>), takeBaseName)
import Data.List (sort)
import Data.Binary.Put (runPut, putCharUtf8, putStringUtf8)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)

import Advent2020.Solve (solverForProblem)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

resultToBS :: ((String, String) -> String) -> Either String PuzzleAnswerPair -> LBS.ByteString
resultToBS _ (Left err) = runPut $ putStringUtf8 err >> putCharUtf8 '\n'
resultToBS f (Right (PuzzleAnswerPair answers)) = runPut $ do
                                                             putStringUtf8 . f $ answers
                                                             putCharUtf8 '\n'

main :: IO ()
main = defaultMain =<< goldenTests

goldenTestForProblemPart :: Int -> Int -> TestTree
goldenTestForProblemPart n p = goldenVsString testName outputFile action
  where
    testName = "Part " ++ show p
    outputFile = "outputs" </> "day_" ++ show n </> "part_" ++ show p <.> "txt"
    action = resultToBS (if p == 1 then fst else snd) <$> solverForProblem n

goldenTestForProblem :: Int -> TestTree
goldenTestForProblem n = testGroup groupName tests
  where
    groupName = "Day " ++ show n
    tests = [goldenTestForProblemPart n 1, goldenTestForProblemPart n 2]

goldenTests :: IO TestTree
goldenTests = do
  inputFiles <- findByExtension [".txt"] "inputs"
  let problemNumbers = sort . map (read . takeBaseName) $ inputFiles
  return . testGroup "Golden Tests" $ map goldenTestForProblem problemNumbers
