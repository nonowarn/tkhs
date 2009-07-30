module Main where

import Tkhs
import Parser

import System.Environment
import System.IO.UTF8 as U
import Codec.Binary.UTF8.String
import Data.Char

main :: IO ()
main = getArgs >>= U.readFile . head
               >>= either (error . show) (runP presentation) . preprocess
    -- vty recognizes any charactor has 1 half-width
    -- thus for non-ascii full-width byte char,
    -- for displaying string at center, some tuning is needed.
    -- concretely, 1 half-width is ignored by 1 full-width char,
    -- thus ignored width is added by adding spaces.
    where preprocess = parseSlides . unlines . map processWideChars . lines

processWideChars :: String -> String
processWideChars str = let wideChars = length . filter (not . isHalfWidth) $ str
                       in str ++ replicate wideChars ' '

-- Table of Half-width Kana
-- http://ash.jp/code/unitbl1.htm
isHalfWidth :: Char -> Bool
isHalfWidth c = isLatin1 c || c `elem` [h .. t]
    where h = head . decodeString $ "\xef\xbd\xa1" -- minimum of half-kana
          t = head . decodeString $ "\xef\xbe\x9f" -- maximum of half-kana
