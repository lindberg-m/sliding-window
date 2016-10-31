{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Read
--import qualified Data.Attoparsec.Text.Lazy

data ParseResult a b = ParseResult { group :: Maybe T.Text,
                                     posit :: Maybe a,
                                     value :: b}
  deriving (Show, Eq)

parseLine :: (Num Integer, Num b, Monad m) =>
              Char ->
              Maybe Int ->
              Maybe Int ->
              Maybe Int ->
              T.Text ->
              ExceptT String m (ParseResult Integer Double)
parseLine delim grCol posCol valCol x = ParseResult <$> getGroup <*> getPos <*> getVal
  where
    parts    = zip [1..] $ T.splitOn (T.pack [delim]) x
    getGroup = case grCol of
      Nothing -> pure Nothing
      Just i  -> fmap Just $ lookupColumn (errMsg "group") i parts

    getPos   = trans $ parseOptionalColumn (errMsg "position") posCol decimal parts
    
    getVal   = do
      v <- trans getVal'
      case v of
        (Just i) -> pure i
        _        -> throwE "Error parsing value, most likely a bug"
        
    getVal'  = case valCol of
      Nothing -> parseOptionalColumn (errMsg "value") (Just 1) double parts
      _       -> parseOptionalColumn (errMsg "value") valCol double parts


    errMsg e      = "Couldn't parse column for " ++ e ++ ". Is the column argument correct?"


-- Optionally apply `Reader a`-parser on xs. Return a lifted Nothing if no column is to be found
parseOptionalColumn :: Ord a => String -> Maybe a -> Reader b -> [(a, T.Text)] -> ExceptT String Maybe b
parseOptionalColumn _ Nothing _ _   = lift Nothing
parseOptionalColumn e (Just i) p xs = lookupColumn e i xs >>= read' p

lookupColumn :: (Ord a, Monad m) => String -> a -> [(a, b)] -> ExceptT String m b
lookupColumn e i xs = case lookup i xs of
  Nothing -> throwE e
  Just a  -> pure a

read' :: Monad m => Reader a -> T.Text -> ExceptT String m a
read' r x = case r x of
  Right (a,_) -> pure a
  Left e      -> throwE $ "Couldn't parse " ++ T.unpack x ++ '.':e

trans :: Monad m => ExceptT e Maybe a -> ExceptT e m (Maybe a)
trans = mapExceptT f where
  f Nothing          = pure $ Right Nothing
  f (Just (Right a)) = pure $ Right (Just a)
  f (Just (Left  e)) = pure $ Left e
