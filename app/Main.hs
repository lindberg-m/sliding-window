module Main where

import System.IO
import System.Exit
import Options.Applicative
import Data.Either
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad                       ((>=>))
import Control.Monad.Trans.Except          (runExcept, runExceptT, Except, ExceptT)
import Control.Monad.Trans.Reader          (runReaderT)

import Window
import Data
import Interface
import Parser


main :: IO ()
main = parseTest


parseTest :: IO ()
parseTest = do
  (opts,content) <- interface
  runExceptT (runReaderT (parseLines content) opts) >>=
          throwError >>=           
          mapM_ (throwError . runExcept >=> print) 

-- TODO
parse :: IO [[Except String (ParseResult Int Double)]]
parse = do
  (opts, content) <- interface
  undefined

interface :: IO (InterfaceOptions, T.Text)
interface = do
  opts <- execParser (info options fullDesc)
  h <- maybe (return stdin)
             (flip openFile ReadMode)
             (infile opts)
  contents <- TIO.hGetContents h
  return (opts, contents)
  

throwError :: Either String a -> IO a
throwError = either (\l -> hPutStrLn stderr l >> exitFailure) return
