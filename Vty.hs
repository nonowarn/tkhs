{-# OPTIONS_GHC -fglasgow-exts #-}

module Vty (
-- * V Monad (Vty actions wrapper)
  V (..), runVty

-- ** Accessing Vty
, event, draw
, size, width, height

-- ** Utilities along V
, centering

-- * A Monad (Attr Combinators)
, A (..), buildAttr

-- ** Attr Combinators
, fg, bg, rv, bold, underline, blink

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
, toPic, render, renderA, centeringBy, Width, Height

, module Graphics.Vty
) where

import Graphics.Vty
import Control.Applicative hiding ((<|>))
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Exception
-- import Codec.Binary.UTF8.String
-- import qualified Data.ByteString.UTF8 as U
import Data.Monoid

newtype V a = V { unV :: ReaderT Vty IO a }
    deriving (Functor, Monad, MonadIO, MonadReader Vty)

newtype A a = A { unA :: Writer (Endo Attr) a }
    deriving (Functor, Monad, MonadWriter (Endo Attr))

type Dispatcher r = (Event, r)
newtype D r a = D { unD :: Writer [Dispatcher r] a }
    deriving (Functor, Monad, MonadWriter [Dispatcher r])

buildAttr :: A a -> Attr
buildAttr (A w) = appEndo (execWriter w) attr

runVty :: V a -> IO a
runVty (V v) = do
  vty <- mkVty
  a <- runReaderT v vty `finally` shutdown vty
  return a

event :: V Event
event = ask >>= liftIO . getEvent

draw :: Picture -> V ()
draw p = ask >>= liftIO . flip update p

type Width = Int
type Height = Int

size :: V (Width,Height)
size = ask >>= liftIO . getSize

width :: V Width
width = fst <$> size

height :: V Height
height = snd <$> size

centering :: Image -> V Image
centering image = do
  (w,h) <- size
  let imgW = imgWidth image
      newImg = if imgW > w
               then image
               else centeringBy w h image
  return newImg

addAttr :: (Attr -> Attr) -> A ()
addAttr = tell . Endo

fg :: Color -> A ()
fg = addAttr . setFG

bg :: Color -> A ()
bg = addAttr . setBG

rv :: A ()
rv = addAttr setRV

underline :: A ()
underline = addAttr setUnderline

blink :: A ()
blink = addAttr setBlink

bold :: A ()
bold = addAttr setBold

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
ascii c = asKeyEvent . KASCII $ c

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
toPic img = pic { pImage = img }

render :: String -> Image
-- render str = let bs = U.pack str
--              in renderBS attr bs
render = horzcat . map (renderChar attr)

renderA :: A a -> String -> Image
renderA at = horzcat . map (renderChar (buildAttr at))

centeringBy :: Width -> Height -> Image -> Image
centeringBy wholeWidth wholeHeight img
    | wholeWidth < imgWidth img || wholeHeight < imgHeight img = img
    | otherwise = let imgW = imgWidth img
                      imgH = imgHeight img
                      magW = wholeWidth - imgW
                      magH = wholeHeight - imgH
                      lpad = magW `div` 2
                      rpad = magW `div` 2 + magW `mod` 2
                      tpad = magH `div` 2
                      bpad = magH `div` 2 + magH `mod` 2
                      spacebox w h  = vertcat
                                    . replicate h
                                    . render $ replicate w ' '
                  in spacebox lpad wholeHeight
                     <|>
                        (spacebox imgW tpad
                     <-> img
                     <-> spacebox imgW bpad)
                     <|>
                     spacebox rpad wholeHeight

