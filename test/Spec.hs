import           Test.Tasty

import qualified Day1Tests
import qualified Day2Tests

tests :: [TestTree]
tests = [
  testGroup "day1" Day1Tests.tests,
  testGroup "day2" Day2Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
