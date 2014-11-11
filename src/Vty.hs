{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vty (
-- * V Monad (Vty actions wrapper)
  V (..), runVty

-- ** Accessing Vty
, event, draw, clear
, size, width, height

-- ** Utilities along V
, centering, doesFit

-- * D Monad (Event Dispatcher Combinators)
, D (..), KeyEvent (..), Dispatcher, toTable, toEvent

-- ** Event and Dispatch Combinators
, onKey, modifiedBy
, waitBy, waitOnce

-- ** Key values working with our types and Synonims for some Modifiers
, ascii, kleft, kup, kdown, kright
, mctrl, malt, mshift

-- ** Lower level interface
, addDispatcher

-- * Utilities
, toPic, render, centeringBy, Width, Height
, doesFitBy

, module Graphics.Vty
) where

import Graphics.Vty
import Control.Applicative hiding ((<|>))
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Exception
import Data.Word
-- import Codec.Binary.UTF8.String
-- import qualified Data.ByteString.UTF8 as U

newtype V a = V { unV :: ReaderT Vty IO a }
    deriving (Functor, Monad, MonadIO, MonadReader Vty)

type Dispatcher r = (Event, r)
newtype D r a = D { unD :: Writer [Dispatcher r] a }
    deriving (Functor, Monad, MonadWriter [Dispatcher r])

runVty :: V a -> IO a
runVty (V v) = do
  vty <- mkVty =<< userConfig
  a <- runReaderT v vty `finally` shutdown vty
  return a

event :: V Event
event = ask >>= liftIO . nextEvent

draw :: Picture -> V ()
draw p = ask >>= liftIO . flip update p

clear :: V ()
clear = ask >>= liftIO . refresh

type Width = Int
type Height = Int

size :: V (Width,Height)
size = do
  output <- asks outputIface
  (w, h) <- liftIO $ displayBounds output
  return (w,h)

width :: V Width
width = fst <$> size

height :: V Height
height = snd <$> size

centering :: Image -> V Image
centering image = do
  (w,h) <- size
  let imgW = imageWidth image
      newImg = if imgW > w
               then image
               else centeringBy w h image
  return newImg

doesFit :: Image -> V Bool
doesFit img = do
  (w,h) <- size
  return . doesFitBy w h $ img

addDispatcher :: Dispatcher r -> D r ()
addDispatcher = tell . (:[])

newtype KeyEvent = KE { unKE :: (Key,[Modifier]) }

onKey :: KeyEvent -> r -> D r ()
onKey ke r = addDispatcher (toEvent ke,r)

toEvent :: KeyEvent -> Event
toEvent = uncurry EvKey . unKE

asKeyEvent :: Key -> KeyEvent
asKeyEvent k = KE (k,[])

ascii :: Char -> KeyEvent
ascii c = asKeyEvent . KChar $ c

modifiedBy :: KeyEvent -> Modifier -> KeyEvent
modifiedBy (KE (k,ms)) m = KE (k,m:ms)

kleft, kright, kup, kdown :: KeyEvent
kleft = asKeyEvent KLeft
kright = asKeyEvent KRight
kup = asKeyEvent KUp
kdown = asKeyEvent KDown

mshift, mctrl, malt :: Modifier
mshift = MShift
mctrl = MCtrl
malt = MAlt

toTable :: D r a -> [(Event,r)]
toTable = execWriter . unD

waitBy :: D r a -> V r
waitBy d = do
  evt <- event
  case tryDispatch evt table of
    Just r -> return r
    Nothing -> waitBy d
    where table = toTable d
          tryDispatch = lookup

waitOnce :: r -> D r a -> V r
waitOnce r d = do
  evt <- event
  return . maybe r id . lookup evt $ toTable d

toPic :: Image -> Picture
toPic img = picForImage img

render :: String -> Image
-- render str = let bs = U.pack str
--              in renderBS attr bs
render = horizCat . map (char defAttr)

centeringBy :: Width -> Height -> Image -> Image
centeringBy wholeWidth wholeHeight img
    | wholeWidth < imageWidth img || wholeHeight < imageHeight img = img
    | otherwise = let imgW = imageWidth img
                      imgH = imageHeight img
                      magW = wholeWidth - imgW
                      magH = wholeHeight - imgH
                      lpad = magW `div` 2
                      rpad = magW `div` 2 + magW `mod` 2
                      tpad = magH `div` 2
                      bpad = magH `div` 2 + magH `mod` 2
                      spacebox w h  = vertCat
                                    . replicate h
                                    . render $ replicate w ' '
                  in spacebox lpad wholeHeight
                     <|>
                         ( spacebox imgW tpad
                       <-> img
                       <-> spacebox imgW bpad )
                     <|>
                     spacebox rpad wholeHeight

doesFitBy :: Width -> Height -> Image -> Bool
doesFitBy w h img = w >= imageWidth img && h >= imageHeight img
