module Interface where

import           Data
import           Window --(Window(..), initWindow)
import           Parser

import qualified Data.Text.Lazy as T
import           Data.List                   (nub)
import           Data.Maybe                  (catMaybes)
import           Control.Monad.Trans.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Options.Applicative


options :: Parser InterfaceOptions
options = InterfaceOptions <$>
   (optional $ strOption
     (short 'f' <>
      long "filename" <>
      metavar "PATH" <>
      help "assumes stdin if omitted")) <*>
   (optional $ strOption
      (short 'd' <>
       long "delim" <>
       metavar "CHAR" <>
       help "assumes tab-delimited file, choose other char if necessary")) <*>
   (optional $ option auto
      (short 'g' <>
       long "groups" <>
       metavar "INT" <>
       help "If a column contain grouping info, specify which column")) <*>
   (optional $ option auto
      (short 'p' <>
       long "position" <>
       metavar "INT" <>
       help" If a column contain positional info, specify which column")) <*>
   (optional $ option auto
      (short 'v' <>
       long "values" <>
       metavar "INT" <>
       help "If multiple columns exist, specify which column contains values of interest")) <*>
  option auto
     (short 's' <>
      long "size" <>
      metavar "INT" <>
      help "Set the size of the window") <*>
  option auto
     (short 't' <>
      long "step" <>
      metavar "INT" <>
      help "Set the step-size of the window") <*>
  switch
    (long "header" <>
     help "Flag: Specify if file contains a header")
   
