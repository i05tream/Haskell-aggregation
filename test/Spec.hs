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
  [ "median test 1" ~: median []                 ~?= Nothing
  , "median test 2" ~: median [1, 2, 3, 4, 5]    ~?= Just 3.0
  , "median test 3" ~: median [1, 2, 3, 4, 5, 6] ~?= Just 3.5
  , "median test 4" ~: median [1, 1, 1, 2, 3]    ~?= Just 2.0
  ]
