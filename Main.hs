module Main where

import Tkhs
import Parser

import System.Environment

main :: IO ()
main =   either (error . show) (runP presentation)
     =<< fmap parseSlides (getArgs >>= readFile . head)

-- main = let (Just slides) = Zipper.fromList [T ["The Title","of","Presentation"]
--                                            ,T ["bar"]
--                                            ,F ["baz","kick","kuzzz"]]
--        in runP presentation slides
