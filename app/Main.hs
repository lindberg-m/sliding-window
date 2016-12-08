module Main where

import qualified Data.Sequence as Seq
import System.IO
import System.IO.Unsafe                    (unsafeInterleaveIO)
import System.Exit
import Options.Applicative                 hiding (value)
import Data.Function                       (on)
import Data.List                           (groupBy, zipWith4)
import Data.Maybe
import Data.Either                         (isRight)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.Identity
import Control.Monad                       ((>=>))
import Control.Monad.IO.Class              (liftIO)
import Control.Monad.Trans.Except          (runExcept, runExceptT, Except, ExceptT(..), throwE)
import Control.Monad.Trans.Reader          (runReaderT, runReader, asks, ReaderT, Reader)

import Window
import Data
import Interface
import Parser


main :: IO ()
main = do
  (opts,content) <- interface
  let snps = runReader (parseTest content) opts
      (successes, failures) = span (isRight) $ map runExcept snps
  

  mapM_ (\(Right a) -> print a) successes
  mapM_ (\(Left a) -> hPutStrLn stderr a >> exitFailure) failures
  
  
--parseTest :: T.Text -> Reader InterfaceOptions [Except String (SNP Double Int)]
parseTest content = do
  res <- parseLines content
  return $ map (throwLineError toSNP) res

{-test res = map (assertOrder . map (f <$> zip [1..]))
          $ groupBy ((==) `on` group) res-}

  
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

third (_,_,x) = x

onThird f (a,b,c) = (a,b, f c)

assertTwoExceptions :: (Monad m) =>
                        e ->
                        (a -> a -> Bool) ->
                        ExceptT e m a ->
                        ExceptT e m a ->
                        ExceptT e m a
assertTwoExceptions e p a b = ExceptT $ do
  f <$> runExceptT a <*> runExceptT b
  where
    f (Right x) (Right y) = if p x y then Right x else Left e
    f x _ = x
    
assertAll :: (Applicative f, Foldable f) => f (a -> b -> Bool) -> a -> b -> Bool
assertAll f x y = and $ f <*> pure x <*> pure y
  
assertOrder :: Ord a => [ParseRes a b] -> [ParseRes a b]
assertOrder = pairMap (\(i,t,x) (j,t',y)->
   let
     e = "Not in order at line " ++
             show i ++
             " and line " ++
             show j ++
             "\n" ++
             T.unpack t ++
             "\n" ++
             T.unpack t'
     p = fmap not <$> assertAll [(==) `on` group, (>) `on` posit]
    in (i, t, assertTwoExceptions e p x y))

pairMap f [] = []
pairMap f [x] = [x]
pairMap f (x:x':xs) =
  f x x' : pairMap f (x':xs)

toSNP x = 
   maybe (throwE "Couldn't access position from parse result")
         (\p -> pure $ SNP (group x) p (value x))
         (posit x)

throwError :: (Show a, Show b) => Except a b -> IO b
throwError x = unsafeInterleaveIO $
    case runExcept x of
      (Left e)  -> do
        hPutStrLn stderr $ show e
        return $ undefined
      (Right a) -> return a

runAllThings =
  ExceptProcess (map (\x -> maybe
                             (throwE "Couldn't access position from parse result")
                             (\p -> pure $ SNP (group x) p (value x))
                             (posit x)))
  <--> undefined
  
