{-# LANGUAGE TemplateHaskell #-}
module Data where

import Data.Label
import qualified Data.Sequence as Seq

type Pos = Int

data Window a  = Window {
    _start :: Pos
  , _end   :: Pos
  , _wData :: Seq.Seq a
} deriving (Show, Eq)

mkLabel ''Window

addWindow :: a -> Window a -> Window a
addWindow x = modify wData (flip (|>) x)

(|>) :: Seq.Seq a -> a -> Seq.Seq a
(|>) = (Seq.|>)
