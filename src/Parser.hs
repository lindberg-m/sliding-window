{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data

import           Data.Typeable
import           Control.Applicative         ((<*>), (<$>))
import           Control.Monad.Trans         (lift)
import           Control.Monad.Reader
import           Control.Monad.Trans.Except  (ExceptT, throwE, mapExceptT, catchE)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as R    (Reader, decimal, double)

type ParseResT m a b = ExceptT String m (ParseResult a b)

parseLines :: (Monad m, Monad n) =>
               T.Text ->
               ReaderT InterfaceOptions m [ParseResT n Int Double]
parseLines xs = do
  h <- asks header
  let f = if h then tail else id
  parseLines' . f . zip [1..] $ T.lines xs

parseLines' :: (Show a, Num a, Monad m, Monad n) =>
               [(a, T.Text)] ->
               ReaderT InterfaceOptions m [ParseResT n Int Double]
parseLines' []         = return []
parseLines' ((i,x):xs) = do
  d <- asks $ maybe "\t" T.pack . sepChar -- Default to tab
  g <- asks groupCol
  p <- asks posCol
  v <- asks $ maybe 1 id . valueCol       -- Default to column 1

  let parts  = zip [1..] $ T.splitOn d x
      res    = ParseResult
                   <$> maybeLookup "group" parts pure g
                   <*> maybeLookup "position" parts (read' R.decimal) p
                   <*> catchLookup "value" parts (read' R.double) v
  rest <- parseLines' xs
  return (res:rest)
    where
       catchLookup e xs f j =
            catchE (lookupColumn j xs >>= f)
                   (\e' -> throwE $ unwords
                               ["Parse error at line", show i, "\n", T.unpack x,
                                "\nCouldn't parse column", show j, "for",
                                 show e, "\n", e'])
       maybeLookup e xs f =
            maybe (pure Nothing)
                  (fmap Just <$> catchLookup e xs f)

       lookupColumn i =
            maybe (throwE $ "Couldn't lookate column " ++ show i)
                  pure
                  . lookup i
                  

read' :: (Monad m, Typeable a) => R.Reader a -> T.Text -> ExceptT String m a
read' r x = case r x of
  Right (a,_) -> pure a
  Left e      -> throwE $ unwords ["Couldn't parse",
                                  quote $ T.unpack x,
                                  "as a", type' x, ":",
                                  e]
    where
      type' = show . typeOf . fst . right . r
      right (Right a) = a
      quote s = concat $ ["\"", s, "\""]

