{-# LANGUAGE OverloadedStrings #-}
module SnpParser where

import Control.Applicative
import qualified Data.Text as T
import Data.Attoparsec.Text

data SNP a = SNP {
  chromosome :: T.Text,
  position   :: Int,
  snpVal     :: a
} deriving (Show, Eq)

data SNPfile a = SNPfile {
  header :: [T.Text],
  snps   :: [SNP a]
} deriving (Show, Eq)

parseSNPfile :: T.Text -> Result (SNPfile Double)
parseSNPfile input = undefined

parseSnp f = do
  chrom <- takeTill isHorizontalSpace
  skipSpace
  pos   <- decimal
  skipSpace
  val   <- f
  return $ SNP chrom pos val

parseSNP = parseSnp double


