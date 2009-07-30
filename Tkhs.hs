{-# OPTIONS_GHC -fglasgow-exts #-}

module Tkhs (
-- * P Monad (Presentation with Vty)
  P (..), runP, liftV

-- ** Presentaion loop
, presentation

-- * Slides
, Slide (..), SlideSet, PictureSet

-- * Zipper
, Zipper
)where

import Vty

import qualified Data.List.PointedList as Zipper
import Data.List.PointedList (PointedList)
import Control.Applicative hiding ((<|>))
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Traversable as T

type Zipper a = PointedList a

data Slide = T [String] | F [String]
type SlideSet = Zipper Slide
type PictureSet = Zipper Picture

newtype P a = P { unP :: StateT PictureSet V a }
    deriving (Functor, Monad, MonadState PictureSet)

slideToImage :: Slide -> Image
slideToImage (T ls) = processBy (flip centeringBy 1) ls
slideToImage (F ls) = processBy ljust                ls
    where ljust maxlen img = let orig_width = imgWidth img
                             in img <|> render (replicate (maxlen - orig_width) ' ')

processBy :: (Int -> Image -> Image) -> [String] -> Image
processBy f ls = let imgs = map render ls
                     maxlen = maximum $ map imgWidth imgs
                 in vertcat . map (f maxlen) $ imgs

slideSetToPictureSet :: SlideSet -> V PictureSet
slideSetToPictureSet = T.mapM $ fmap toPic
                              . centering
                              . slideToImage

runP :: P a -> SlideSet -> IO a
runP (P st) slides = runVty $ do
                       ourVty <- ask
                       pictures <- slideSetToPictureSet slides
                       evalStateT st pictures `withVty` ourVty

liftV :: V a -> P a
liftV = P . lift

presentation :: P ()
presentation = do
  current <- Zipper.focus <$> get
  liftV clear
  liftV . draw $ current
  control

control :: P ()
control = id =<< dispatch
    where dispatch = liftV . waitBy $ do
            onKey (ascii 'n') $ loopBy goNext
            onKey kright      $ loopBy goNext
            onKey (ascii 'p') $ loopBy goPrev
            onKey kleft       $ loopBy goPrev
            onKey (ascii 'q') quit
          loopBy = (>> presentation)

goNext :: P ()
goNext = modify (\s -> maybe s id (Zipper.next s))

goPrev :: P ()
goPrev = modify (\s -> maybe s id (Zipper.previous s))

quit :: P ()
quit = return ()

withVty :: V a -> Vty -> V a
withVty (V v) vty = liftIO $ runReaderT v vty
