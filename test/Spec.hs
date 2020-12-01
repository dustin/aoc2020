import           Test.Tasty

import qualified Day1Tests

tests :: [TestTree]
tests = [
  testGroup "day1" Day1Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
