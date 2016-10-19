{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Window where

import Control.Monad.State
import Data.Function (on)
import Data.Sequence (Seq, (|>), dropWhileL)

-- Data Definitions
data Positional a b  where
  Positional :: Ord b => a -> b -> Positional a b 

data Window a b = Window { start  :: a,
                           end    :: a,
                           values :: Seq b }
    deriving (Eq, Show)

data Coverage = Under
              | Overlap
              | Above 
   deriving (Eq, Show)

  
{- Main functions -}
slidingWindow :: (Num a, Monad m, HasPosition p a) =>
                  a ->
                  [p a] ->
                  StateT (Window a (p a)) m [(Window a (p a))]
slidingWindow _ []          = get >>= return . return
slidingWindow step l@(x:xs) = do
  window <- get
  let startpos = start window
      endpos   = end window

  case spanPosition startpos endpos x of
    Under    -> slidingWindow step xs
    Overlap  -> addToWindow x >> slidingWindow step xs
    Above    -> do
      modify (stepWindow step)
      rest <- slidingWindow step l
      return (window:rest)

addToWindow :: (Monad m) => a -> StateT (Window b a) m ()
addToWindow x = do
  vals <- fmap values get
  modify (\st -> st{values = vals |> x})

stepWindow :: (Num a, HasPosition p a) => a -> Window a (p a) -> Window a (p a)
stepWindow x (Window s e v) = Window s' e' v'
    where
      s' = s + x
      e' = e + x
      v' = dropWhileL (s' `pLT`) v

{- Class definition -}
class Ord a => HasPosition p a where
  {- MINIMAL position -}
  position     :: p a -> a
  pCompare     :: p a -> p a -> Ordering
  pLT          :: a -> p a -> Bool
  pGT          :: a -> p a -> Bool
  pEQ          :: a -> p a -> Bool
  ppLT         :: p a -> p a -> Bool
  ppGT         :: p a -> p a -> Bool
  ppEQ         :: p a -> p a -> Bool
  spanPosition :: Ord a => a -> a -> p a -> Coverage

  pCompare = compare `on` position
  pLT p x  = p < (position x)
  pGT p x  = p > (position x)
  pEQ p x  = p == (position x)
  ppLT     = (<) `on` position
  ppGT     = (>) `on` position
  ppEQ     = (==) `on` position

  spanPosition l u p = ordIf (p' `between` (l,u)) Under Overlap Above
    where
      p' = position p
      ordIf LT x _ _ = x
      ordIf EQ _ x _ = x
      ordIf GT _ _ x = x   
      between x (a,b)
        | x < a     = LT
        | otherwise = x `compare` b


-- Instances
instance (Eq a, Ord b) => Eq (Positional a b) where
  (Positional x y) == (Positional x' y') =
    x == x' && y == y'
instance (Show a, Show b) => Show (Positional a b) where
  show (Positional a b) = "Positional " ++ show a ++ ' ':show b
          
instance Ord x => HasPosition (Positional a) x where
  position = pos

{- getters for Positional data type -}
pos :: Positional a b -> b
pos (Positional _ b) = b

val :: Positional a b -> a
val (Positional a _) = a

