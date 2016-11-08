module Main where

import System.IO
import System.Exit
import Options.Applicative
import Data.Either
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad                       ((>=>))
import Control.Monad.Trans.Except          (runExcept, runExceptT)
import Control.Monad.Trans.Reader          (runReaderT)

import Window
import Data
import Interface
import Parser


main :: IO ()
main = parseTest


parseTest :: IO ()
parseTest = do
  (opts,h) <- interface
  content <- TIO.hGetContents h
  runExceptT (runReaderT (parseLines content) opts) >>=
          throwError >>=           
          mapM_ (throwError . runExcept >=> print) 


interface :: IO (InterfaceOptions, Handle)
interface = do
  opts <- execParser (info options fullDesc)
  h <- maybe (return stdin)
             (flip openFile ReadMode)
             (infile opts)
  return (opts, h)
  

throwError :: Either String a -> IO a
throwError = either (\l -> hPutStrLn stderr l >> exitFailure) return
