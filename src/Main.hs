module Main where

import Tkhs
import Parser

import System.Environment
import System.IO.UTF8 as U
import qualified Zipper
import Data.Maybe

main :: IO ()
main = getArgs >>= U.readFile . headOrUsage
               >>= either (error . show)
                          (runP presentation . fromJust . Zipper.fromList . (++[T ["[End of Slide]"]]))
                         . parseSlides

headOrUsage :: [String] -> String
headOrUsage ls | null ls = error "Usage: tkhs [presentation]"
               | otherwise = head ls
