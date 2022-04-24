import Test.HUnit
import Lib


main :: IO ()
main = do
  runTestTT $ TestList
    [ aggregationTest ]
  return ()

aggregationTest = TestList
  [ averageTest
  , medianTest
  ]

averageTest = TestList
  [ "average test 1" ~: average [1, 2, 3] ~?= Just 2
  , "average test 2" ~: average []        ~?= Nothing
  ]

medianTest = TestList
  [ "median test 1" ~: median [] ~?= Nothing ]
