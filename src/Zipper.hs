module Zipper where

import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Monoid

data Zipper a = Zipper { prevs :: [a], current :: a, nexts :: [a] }
                deriving (Eq,Show)

focus :: Zipper a -> a
focus = current

next, previous :: Zipper a -> Maybe (Zipper a)
next     (Zipper _ _ [])      = Nothing
next     (Zipper ps c (n:ns)) = Just $ Zipper (c:ps) n ns
prev     (Zipper [] _ _)      = Nothing
previous (Zipper (p:ps) c ns) = Just $ Zipper ps p (c:ns)

fromList :: [a] -> Maybe (Zipper a)
fromList [] = Nothing
fromList (a:as) = Just $ Zipper [] a as

instance Functor Zipper where
  fmap f (Zipper ps c ns) = Zipper (fmap f ps) (f c) (fmap f ns)

instance Traversable Zipper where
  traverse f (Zipper ps c ns) =   Zipper
                              <$> (reverse <$> traverse f (reverse ps))
                              <*> f c
                              <*> (traverse f ns)

instance Foldable Zipper where
  foldMap f (Zipper ps c ns) = foldMap f ps `mappend` f c `mappend` foldMap f ns
