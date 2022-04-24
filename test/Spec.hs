main :: IO ()
main = putStrLn "Test suite not yet implemented"
import Test.HUnit
import Lib


main :: IO ()
main = do
  runTestTT $ TestList [
    
  ]

aggregationTest = TestList
  [ "average test 1" ~: average [1, 2, 3] ~?= Just 2.0
  , "average test 2" ~: average []        ~?= Nothing
  ]