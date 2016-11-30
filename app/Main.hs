module Main where

import qualified Data.Sequence as Seq
import System.IO
import System.Exit
import Options.Applicative                 hiding (value)
import Data.Function                       (on)
import Data.List                           (groupBy, zipWith4)
import Data.Maybe
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
--main = parseTest2 >>= process >>= mapM_ print
--main = parseTest2
main = do
  (opts,content) <- interface
  parseTest3 content opts
{-  res <- parseTest2
  mapM_ (throwError. runExcept >=> print) res
  -}
  

parseTest :: IO ()
parseTest = do
  (opts,content) <- interface
  let parsed = runReader (parseLines content) opts
  mapM_ (throwError . runExcept . (\(_,_,x) -> x) >=> print) parsed


parseTest2 = do
  (opts, content) <- interface
  let parsed  = assertOrder $ runReader (parseLines content) opts
  mapM_ (throwError . runExcept . third >=> print) parsed

--parseTest3 :: T.Text -> ReaderT InterfaceOptions IO ()
parseTest3 content opts = do
  let parsed = runReader (parseLines content) opts
  mapM_ (throwError . runExcept . third >=> print) parsed

  
showWinLen (a,[]) = return ()
showWinLen (a,(w:ws)) = do
  putStr $ show a ++ " : "
  putStr . show $ start w
  putStr " - "
  putStr . show $ end w
  putStr "  -->>   "
  putStrLn . show . Seq.length $ values w
  showWinLen (a,ws)
  
    
-- A bit hacky maybe
--putGroups :: [ParseResult Int b] -> [(Maybe T.Text, [Positional b Int])]
putGroups content = do
   is <- asks posCol
   return . groupBy ((==) `on` fst) $
     if isNothing is
       then 
         zipWith3
            (\i g v -> (g, Positional v i))
            [1..]
            (map group content)
            (map value content)
       else
         map
           (\x -> (group x, Positional (value x) (maybe undefined id $ posit x)))
           $content
         

interface :: IO (InterfaceOptions, T.Text)
interface = do
  opts <- execParser (info options fullDesc)
  h <- maybe (return stdin)
             (flip openFile ReadMode)
             (infile opts)
  contents <- TIO.hGetContents h
  return (opts, contents)
  


{- Misc Functions -}

throwError = either (\l -> hPutStrLn stderr l >> exitFailure) return

third (_,_,x) = x

compareTwoExceptions :: (Monad m) =>
                        e ->
                        (a -> a -> Bool) ->
                        ExceptT e m a ->
                        ExceptT e m a ->
                        ExceptT e m a
compareTwoExceptions e p a b = ExceptT $ do
  f <$> runExceptT a <*> runExceptT b
  where
    f (Right x) (Right y) = if p x y then Right x else Left e
    f x _ = x
    
assertAll :: (Applicative f, Foldable f) => f (a -> b -> Bool) -> a -> b -> Bool
assertAll f x y = and $ f <*> pure x <*> pure y
  
assertOrder parsed =
  pairMap (\(i,t,x) (j,t',y)->
             (i, t,
               compareTwoExceptions
                  ("Not in order at line " ++
                      show i ++
                      "\n" ++
                      T.unpack t ++
                      "\n" ++
                      T.unpack t')
                  (fmap not <$> assertAll [(==) `on` group, (>) `on` posit])
                  x
                  y)) parsed

pairMap f [] = []
pairMap f [x] = [x]
pairMap f (x:x':xs) =
  f x x' : pairMap f (x':xs)
