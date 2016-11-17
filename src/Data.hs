{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data where

import Data.Text.Lazy as T (Text)
import Data.Sequence       (Seq, (|>))
import Data.Function       (on)
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Identity
--import Control.Monad.Tr


--type App2 a = ReaderT InterfaceOptions (Except String) a
type App m a = ReaderT InterfaceOptions (ExceptT String m) a
--type App2 t a = ReaderT InterfaceOptions (t (ExceptT String Identity)) a


getAppT :: Monad m => App m a -> InterfaceOptions -> m (Either String a)
getAppT r = runExceptT . runReaderT r
getApp :: App Identity a -> InterfaceOptions -> Either String a
getApp  r = runExcept . runReaderT r

data ParseResult a b = ParseResult {
  group :: Maybe T.Text,
  posit :: Maybe a,
  value :: b
  } deriving (Show, Eq)

data InterfaceOptions =
  InterfaceOptions { infile    :: Maybe String,
                     sepChar   :: Maybe String,
                     groupCol  :: Maybe Int,
                     posCol    :: Maybe Int,
                     valueCol  :: Maybe Int,
                     wSize     :: Int,
                     wStep     :: Int,
                     header    :: Bool}
  deriving (Show, Eq)

data Positional a b = Positional {val :: a, pos :: b}

data Window a b = Window { start  :: a,
                           end    :: a,
                           values :: Seq b }
    deriving (Eq, Show)

data Coverage = Under
              | Overlap
              | Above 
   deriving (Eq, Show)

class Ord a => HasPosition p a where
  {- MINIMAL position -}
  position  :: p a -> a
  compareP  :: p a -> p a -> Ordering
  onP       :: (a -> b) -> p a -> b
  onP2      :: (a -> a -> b) -> p a -> p a -> b

  onP f     = f . position
  onP2 f    = f `on` position
  compareP  = onP2 compare


-- Instances
instance (Eq a, Eq b) => Eq (Positional a b) where
  (Positional x y) == (Positional x' y') =
    x == x' && y == y'
instance (Show a, Show b) => Show (Positional a b) where
  show (Positional a b) = "Positional " ++ show a ++ ' ':show b
          
instance Ord x => HasPosition (Positional a) x where
  position = pos


