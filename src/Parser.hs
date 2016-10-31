module Parser (
  ParseResult(..),
  parseLine) where

import           Control.Applicative         ((<*>), (<$>))
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Except  (ExceptT, throwE, mapExceptT)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Read         (Reader, decimal, double)


data ParseResult a b = ParseResult { group :: Maybe T.Text,
                                     posit :: Maybe a,
                                     value :: b}
  deriving (Show, Eq)

parseLine :: (Num b, Monad m) =>
              Char ->
              Maybe Int ->
              Maybe Int ->
              Int ->
              T.Text ->
              ExceptT String m (ParseResult Integer Double)
parseLine delim grCol posCol valCol x = ParseResult <$> getGroup <*> getPos <*> getVal
  where
    parts    = zip [1..] $ T.splitOn (T.pack [delim]) x -- split up the line and make it searchable
    getGroup = maybeLookupColumn grCol "group" pure
    getPos   = maybeLookupColumn posCol "position" (read' decimal) 
--    getPos   = maybe (pure Nothing)
--                     (\x -> Just <$> (lookupColumn (errMsg "position") x parts >>= read' decimal))
--                     posCol

    getVal   = lookupColumn (errMsg "value") valCol parts >>= read' double
    errMsg e = "Couldn't parse column for " ++ e ++ ". Is the column argument correct?"

    maybeLookupColumn col e f = maybe (pure Nothing)
                                      (\x -> Just <$> (lookupColumn (errMsg e) x parts >>= f))
                                       col

lookupColumn :: (Ord a, Monad m) => String -> a -> [(a, b)] -> ExceptT String m b
lookupColumn e i xs = case lookup i xs of
  Nothing -> throwE e
  Just a  -> pure a

read' :: Monad m => Reader a -> T.Text -> ExceptT String m a
read' r x = case r x of
  Right (a,_) -> pure a
  Left e      -> throwE $ "Couldn't parse " ++ T.unpack x ++ '.':e

