import Test.HUnit
import Aggregation


main :: IO ()
main = do
  runTestTT $ TestList
    [ aggregationTest ]
  return ()

aggregationTest = TestList
  [ isNumberTest
  , averageTest
  , medianTest
  , uniqueTest
  ]

averageTest = TestList
  [ "average test 1" ~: average [1, 2, 3] ~?= Just 2.0
  , "average test 2" ~: average []        ~?= Nothing
  ]

medianTest = TestList
  [ "median test 1" ~: median []                 ~?= Nothing
  , "median test 2" ~: median [1, 2, 3, 4, 5]    ~?= Just 3.0
  , "median test 3" ~: median [1, 2, 3, 4, 5, 6] ~?= Just 3.5
  , "median test 4" ~: median [1, 1, 1, 2, 3]    ~?= Just 2.0
  ]

uniqueTest = TestList
  [ "unique test 1" ~: unique [] ~?= ([] :: [Int])
  , "unique test 2" ~: unique [1, 2, 1, 1, 2] ~?= ([1, 2] :: [Int])
  ]

isNumberTest = TestList
  [ "isNumber test 1" ~: isNumber "123" ~?= True
  , "isNumber test 2" ~: isNumber "0"   ~?= True
  , "isNumber test 3" ~: isNumber "1a"  ~?= False
  ]
