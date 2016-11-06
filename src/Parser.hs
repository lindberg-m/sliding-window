{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data

import           Control.Applicative         ((<*>), (<$>))
import           Control.Monad.Trans         (lift)
import           Control.Monad.Reader
import           Control.Monad.Trans.Except  (ExceptT, throwE, mapExceptT, catchE)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as R    (Reader, decimal, double)


type ParseReaderT    = ReaderT InterfaceOptions
type ParseResT m a b = ExceptT String m (ParseResult a b)

parseLines :: (Monad m, Monad n) => T.Text -> ParseReaderT m [ParseResT n Int Double]
parseLines xs = do
  h <- asks header
  let f = if h then id else tail
  parseLines' . f . zip [1..] $ T.lines xs

parseLines' :: (Show a, Num a, Monad m, Monad n) =>
               [(a, T.Text)] ->
               ParseReaderT m [ParseResT n Int Double]
parseLines' []         = return []
parseLines' ((i,x):xs) = do
  res  <- parseLine x
  rest <- parseLines' xs
  return (res:rest)
    where
      parseLine l = do
        (d,g,p,v) <- (,,,) <$> delim <*> asks groupCol <*> asks posCol <*> valcol
        let parts  = zip [1..] $ T.splitOn d l
            gr     = maybeLookup g (errMsg "group") parts pure
            pos    = maybeLookup p (errMsg "position") parts (read' R.decimal)
            val    = lookupColumn (errMsg "value") v parts >>= read' R.double  
        return $ ParseResult <$> gr <*> pos <*> val

      delim  = asks sepChar >>= return . maybe "\t" T.pack -- default to tab
      valcol = asks valueCol >>= return . maybe 1 id       -- default to 1
      maybeLookup c m x f =
          maybe (pure Nothing) (\i -> Just <$> (lookupColumn m i x >>= f)) c
      errMsg e = "Parse error at line " ++
                 show i ++ "\nCouldn't parse column for " ++
                 show e ++ ". Is the column argument correct?"

lookupColumn :: (Ord a, Monad m) => e -> a -> [(a, b)] -> ExceptT e m b
lookupColumn e i xs = case lookup i xs of
  Nothing -> throwE e
  Just a  -> pure a

read' :: Monad m => R.Reader a -> T.Text -> ExceptT String m a
read' r x = case r x of
  Right (a,_) -> pure a
  Left e      -> throwE $ "Couldn't parse " ++ T.unpack x ++ '.':'\n':e


--- testing

{-
parseLines'' []         = return []
parseLines'' ((i,x):xs) = do
  d      <- return . maybe "\t" T.pack =<< asks sepChar
  v      <- return . maybe 1 id =<< asks valueCol
  g      <- asks groupCol
  p      <- asks posCol
  let parts = zip [1..] $ T.splitOn d x
      val = maybe (throwE $ errMsg "value")
                  (\x -> catchE (read' R.double x) (\e -> errMsg "value" ++ e))
                  (lookup v parts)
      gr  = maybe (pure Nothing)
                  (lookupColumn' "group" pure) g
      pos = maybe (pure Nothing) (lookupColumn' "position" (read' R.decimal)) p
      errMsg e = "Parse error at line " ++ show i ++ " when looking for " ++ e

{-  fGroup <- maybe (pure Nothing) (lookupCol pure) =<< asks groupCol
  fPos   <- maybe (pure Nothing) (lookupCol (read' decimal)) =<< asks posCol
  let parts          = zip [1..] $ T.splitOn d x
      lookupCol f i' = case lookup i' parts of
        Just a -> Just $ f a
        _      -> Just . throwE ("Parse error at line " ++ show i ++
                                 " when parsing column " ++ show i' ++
                                 ".\nCouldn't find column")
      val = catchE 
      -}
    

-- Helper functions
lookupColumn' :: (Eq a, Show a, Monad m, Monad n) =>
                 [(a,b)] ->
                 (r -> Maybe a) ->
                 e ->
                 ReaderT r m (ExceptT e n (Maybe b))
lookupColumn' x f e = do
  i <- asks f
  case i of
    Nothing -> pure Nothing
    Just a  -> case lookup a x of
      Nothing -> return $ throwE $ e ++ "\nError finding column " ++ show a
      Just b  -> pure $ Just b
-}
