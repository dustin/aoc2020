import           Test.Tasty

import qualified Day1Tests
import qualified Day2Tests
import qualified Day3Tests
import qualified Day4Tests
import qualified Day5Tests
import qualified Day6Tests
import qualified Day7Tests

tests :: [TestTree]
tests = [
  testGroup "day1" Day1Tests.tests,
  testGroup "day2" Day2Tests.tests,
  testGroup "day3" Day3Tests.tests,
  testGroup "day4" Day4Tests.tests,
  testGroup "day5" Day5Tests.tests,
  testGroup "day6" Day6Tests.tests,
  testGroup "day7" Day7Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
