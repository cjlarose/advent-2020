import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>), (<.>), takeBaseName)
import Data.List (sort)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)

import Advent2020.Solve (solve)

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
