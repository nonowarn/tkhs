import Vty

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.API (Test)
import Test.HUnit (Assertable(..))

main = defaultMain [
        testGroup "doesFitBy" [
         "returns True if an image seemed to fit" ~~
          doesFitBy 100 100 (render "foo")
        ,"returns False unless an image seemed to fit" ~~
          not (doesFitBy   0   0 (render "foo"))
        ,"returns True if width and height is equal to image's width height" ~~
          let img = render "foo"; w = imgWidth img; h = imgHeight img
          in doesFitBy w h img
        ]
       ]

(~~) :: (Assertable a) => String -> a -> Test
testname ~~ assertion = testCase testname . assert $ assertion
