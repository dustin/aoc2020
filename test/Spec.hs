import           Test.Tasty

import qualified Day1Tests
import qualified Day2Tests
import qualified Day3Tests
import qualified Day4Tests

tests :: [TestTree]
tests = [
  testGroup "day1" Day1Tests.tests,
  testGroup "day2" Day2Tests.tests,
  testGroup "day3" Day3Tests.tests,
  testGroup "day4" Day4Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
