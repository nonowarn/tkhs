module Main where

import Tkhs
import Parser

import System.Environment

main :: IO ()
main =   either (error . show) (runP presentation)
     =<< fmap parseSlides (getArgs >>= readFile . head)

