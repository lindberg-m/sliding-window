module Main where

import qualified Data.Sequence as Seq
import System.IO
import System.Exit
import Options.Applicative                 hiding (value)
import Data.Function                       (on)
import Data.List                           (groupBy, zipWith4)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.Identity
import Control.Monad                       ((>=>))
import Control.Monad.IO.Class              (liftIO)
import Control.Monad.Trans.Except          (runExcept, runExceptT, Except, ExceptT(..), throwE)
import Control.Monad.Trans.Reader          (runReaderT, runReader, asks, ReaderT)

import Window
import Data
import Interface
import Parser


main :: IO ()
main = parseTest2


parseTest :: IO ()
parseTest = do
  (opts,content) <- interface
  runExceptT (runReaderT (parseLines content) opts) >>=
          throwError >>=           
          mapM_ (throwError . runExcept >=> print) 

-- TODO
--parse :: IO [[Except String (ParseResult Int Double)]]
parseTest2 = do
  (opts, content) <- interface
  parsed <- process $ runReader (parseLines content) opts
  undefined
--  windows <- runReaderT (putWindows content) opts
--  mapM_ showWinLen windows
  
showWinLen (a,[]) = return ()
showWinLen (a,(w:ws)) = do
  putStr $ show a ++ " : "
  putStr . show $ start w
  putStr " - "
  putStr . show $ end w
  putStr "  -->>   "
  putStrLn . show . Seq.length $ values w
  showWinLen (a,ws)


makeGroups :: [ParseResult Int b] ->
             [[Except String (Maybe T.Text, Positional b Int)]]
makeGroups xs = undefined
  
    
-- A bit hacky maybe
--putGroups :: [ParseResult Int b] -> [(Maybe T.Text, [Positional b Int])]
putGroups content =
--   map (\xs -> (fst (head xs), map snd xs)) .
   groupBy ((==) `on` fst) $
       zipWith4
          (\i g v p -> (g, Positional v $ maybe i id p))
          [1..]
          (map group content)
          (map value content)
          (map posit content)
   

interface :: IO (InterfaceOptions, T.Text)
interface = do
  opts <- execParser (info options fullDesc)
  h <- maybe (return stdin)
             (flip openFile ReadMode)
             (infile opts)
  contents <- TIO.hGetContents h
  return (opts, contents)
  
process :: [Except String a] -> IO [a]
process = mapM (throwError . runExcept)

throwError = either (\l -> hPutStrLn stderr l >> exitFailure) return

compareTwoExceptions :: (Monad m) =>
                        e ->
                        (a -> a -> Bool) ->
                        ExceptT e m a ->
                        ExceptT e m a ->
                        ExceptT e m a
compareTwoExceptions e p a b = ExceptT $ do
  f <$> runExceptT a <*> runExceptT b
  where
    f (Right x) (Right y) = if p x y
                            then Right x
                            else Left e
    f x _ = x
    
assertAll :: (Applicative f, Foldable f) => f (a -> b -> Bool) -> a -> b -> Bool
assertAll f x y = and $ f <*> pure x <*> pure y
  
  
