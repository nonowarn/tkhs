{-# OPTIONS_GHC -fglasgow-exts -XNoMonomorphismRestriction #-}
{-# OPTIONS_GHC -XPackageImports #-}

module Parser (parseSlides) where

import Tkhs

import Text.Parsec hiding (newline, (<|>))
import Control.Monad.Identity
import Control.Applicative hiding (many)
import Data.Maybe
import Data.List.PointedList as Zipper

import Prelude hiding (lines)

parseSlides :: String -> Either ParseError SlideSet
parseSlides str = parse (fromJust . Zipper.fromList <$> many1 slide) "" str

type PC a = ParsecT String () Identity a

t_sig, f_sig, any_sig :: PC ()
t_sig = char '-' >> newline >> return ()
f_sig = char '=' >> newline >> return ()
any_sig = choice [t_sig,f_sig]

t :: PC Slide
t = do
  t_sig
  T <$> lines

f :: PC Slide
f = do
  f_sig
  F <$> lines

lines :: PC [String]
lines = line `manyTill` (lookAhead (any_sig <|> eof))

slide :: PC Slide
slide = try t <|> try f <?> "slide"

line :: PC String
line = noneOf "\r\n" `manyTill` try newline

newline :: PC String
newline =   try (string "\r\n")
        <|> try (string "\n")
        <|> try (string "\r")
        <?> "newline"
