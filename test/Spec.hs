import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>), (<.>), takeBaseName)
import Data.List (sort)
import Data.Binary.Put (runPut, putCharUtf8, putStringUtf8)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)

import Advent2020.Solve (solver)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

solve :: Int -> IO LBS.ByteString
solve k = do
  res <- solver k
  let bs = case res of
             Left err -> runPut $ putStringUtf8 err >> putCharUtf8 '\n'
             Right (PuzzleAnswerPair (part1, part2)) -> runPut $ do
                                                          putStringUtf8 part1
                                                          putCharUtf8 '\n'
                                                          putStringUtf8 part2
                                                          putCharUtf8 '\n'
  pure bs

main :: IO ()
main = defaultMain =<< goldenTests

goldenTestForProblem :: Int -> TestTree
goldenTestForProblem n = goldenVsString testName outputFile action
  where
    testName = "Day " ++ show n
    outputFile = "outputs" </> show n <.> "txt"
    action = solve n

goldenTests :: IO TestTree
goldenTests = do
  inputFiles <- findByExtension [".txt"] "inputs"
  let problemNumbers = sort . map (read . takeBaseName) $ inputFiles
  return . testGroup "Golden Tests" $ map goldenTestForProblem problemNumbers
