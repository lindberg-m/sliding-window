{-# LANGUAGE OverloadedStrings #-}
module Interface where

import           Window (Window(..), initWindow)
import           Parser

import qualified Data.Text.Lazy as T
import           Data.List                   (nub)
import           Data.Maybe                  (catMaybes)
import           Control.Monad.Trans.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Options.Applicative

data InterfaceOptions =
  InterfaceOptions { infile   :: Maybe String,
                     sepChar  :: Maybe String,
                     groups   :: Maybe Int,
                     position :: Maybe Int,
                     iValues   :: Maybe Int,
                     wSize    :: Integer,
                     wStep    :: Integer,
                     iHeader   :: Bool}
  deriving (Show, Eq)

newtype FileParserT m a = FileParserT { parse :: [T.Text] -> [ExceptT String m a] }
type FileParser = FileParserT Identity
type Interface m a = ReaderT InterfaceOptions (ExceptT String m) a

getStartWindow :: (Monad m) => Interface m (Window Integer b)
getStartWindow = do
  size <- asks wSize
  return $ initWindow size

getFileParser :: (Monad m) => Interface m (FileParser (ParseResult Integer Double))
getFileParser = do
  separator <- asks sepChar
  groupCol  <- asks groups
  posCol    <- asks position
  valCol    <- asks iValues
  hasHead   <- asks iHeader
  guardColumns valCol groupCol posCol
  return $ FileParserT (map (lineParser' separator groupCol posCol valCol) . (skipHead hasHead) . zip [1..])
  
  where
    skipHead h = if h then drop 1 else id
    lineParser' s g p v =
          let parser = parseLine (maybe (T.pack "\t") T.pack s) g p (maybe 1 id v)
          in (\(i,x) -> catchE (parser x) (\e -> throwE $ "Parse error at line " ++ show i ++ ". " ++ e))
          
     
    guardColumns Nothing Nothing Nothing = pure ()
    guardColumns Nothing _ _             = lift $ throwE
                                                  ("Need to know what column to use"  ++
                                                   " as values when columns for " ++
                                                   "position or groups are specified")
    guardColumns a b c
      | nub xs == xs = pure ()
      | otherwise    = lift (throwE
                            "Cannot have the same column specified multiple times")
      where
        xs = catMaybes [a,b,c]
      
  
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
   
      
      
         
--windowingFunction :: Handle -> [WindowStats]
--windowingFunction = undefined

