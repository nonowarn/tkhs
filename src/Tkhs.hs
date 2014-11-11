{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Vty hiding (text)

import qualified Zipper
import Zipper (Zipper)
import Control.Applicative hiding ((<|>))
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import System.IO
import System.Exit
import Text.PrettyPrint hiding (render)

data Slide = T [String] | F [String]
type SlideSet = Zipper Slide
type PictureSet = Zipper Picture

newtype P a = P { unP :: StateT PictureSet V a }
    deriving (Functor, Applicative, Monad, MonadState PictureSet)

slideToImage :: Slide -> Image
slideToImage (T ls) = processBy (flip centeringBy 1 . fromIntegral) ls
slideToImage (F ls) = processBy ljust                ls
    where ljust maxlen img = let origWidth = imageWidth img
                             in img <|> render (replicate (maxlen - fromIntegral origWidth) ' ')

processBy :: (Int -> Image -> Image) -> [String] -> Image
processBy f ls = let imgs = map render ls
                     maxlen = fromIntegral . maximum $ map imageWidth imgs
                 in vertCat . map (f maxlen) $ imgs

-- slideSetToPictureSet :: SlideSet -> V PictureSet
-- slideSetToPictureSet = T.mapM $ fmap toPic
--                               . centering
--                               . slideToImage

runP :: P a -> SlideSet -> IO a
runP (P st) slides = runVty $ do
   ourVty <- ask
   let imgset = fmap slideToImage slides
   check <- F.and <$> T.mapM doesFit imgset
   when (not check) $ do
     let maxWidth = F.maximum $ fmap imageWidth imgset
         maxHeight = F.maximum $ fmap imageHeight imgset
     mapM_ warn [ "To display this presentation, the terminal must be at least "
                      ++ show maxWidth ++ "x" ++ show maxHeight ++ "."
                , "Please try again with a bigger terminal."
                , "Press any key to exit." ]
     liftIO =<< waitOnce exitFailure (return ())
   pictures <- T.mapM (fmap toPic . centering) imgset
   evalStateT st pictures `withVty` ourVty

warn :: String -> V ()
warn str = do
  w <- width
  liftIO . hPutStrLn stderr
         . renderStyle style { lineLength = fromIntegral w, ribbonsPerLine = 1.0 }
         . fsep . map text . words $ str

liftV :: V a -> P a
liftV = P . lift

presentation :: P ()
presentation = do
  current <- Zipper.focus <$> get
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
