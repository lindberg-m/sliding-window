{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data

import           Data.Typeable
import           Data.Either
import           Control.Applicative         ((<*>), (<$>))
import           Control.Monad.Trans         (lift)
import           Control.Monad.Reader
import           Control.Monad.Trans.Except  (ExceptT, throwE, mapExceptT, catchE)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as R    (Reader, decimal, double)


type ParseResT m a b = ExceptT String m (ParseResult a b)
--type ParseFix m n a = ReaderT InterfaceOptions m [ParseResT n Int Double]

parseLines :: (Monad m, Monad n) => T.Text -> App m [ParseResT n Int Double]
parseLines xs = do
  conf <- ask
  let f = if (header conf) then tail else id
  return (runReader (parseLines' . f . zip [1..] $ T.lines xs) conf)

parseLines' :: (Show a, Num a, Monad m, Monad n) =>
               [(a, T.Text)] ->
               ReaderT InterfaceOptions m [ParseResT n Int Double]
parseLines' []         = return []
parseLines' ((i,x):xs) = do
  res  <- parseLine x
  rest <- parseLines' xs
  return (res:rest)
    where
      parseLine l = do
        (d,g,p,v) <- (,,,) <$> delim <*> asks groupCol <*> asks posCol <*> valcol
        let parts  = zip [1..] $ T.splitOn d l
            gr     = maybeLookup g "group" parts pure
            pos    = maybeLookup p "position" parts (read' R.decimal)
            val    = catchE (lookupColumn v parts >>= (read' R.double))
                            (error' "value")
        return $ ParseResult <$> gr <*> pos <*> val

      delim  = asks sepChar >>= return . maybe "\t" T.pack -- default to tab
      valcol = asks valueCol >>= return . maybe 1 id       -- default to 1
      maybeLookup c m xs f =
          maybe (pure Nothing)
          (\i -> Just <$>
                 (catchE (lookupColumn i xs >>= f) (error' m)))
           c
      error' e1 e2 = throwE $
           (unwords ["Parse error at line", show i, "\n", T.unpack x,
                     "\nCouldn't parse column for",show e1, "\n"]) ++ e2

lookupColumn :: (Ord a, Show a, Monad m) => a -> [(a, b)] -> ExceptT String m b
lookupColumn i xs = case lookup i xs of
  Nothing -> throwE $ "Couldn locate column " ++ show i
  Just a  -> pure a

read' :: (Monad m, Typeable a) => R.Reader a -> T.Text -> ExceptT String m a
read' r x = case r x of
  Right (a,_) -> pure a
  Left e      -> throwE $ unwords ["Couldn't parse",
                                  quote $ T.unpack x,
                                  "as a",
                                  type',
                                  ":",
                                  e]
    where
      type'   = show . typeOf . fst . head $ rights [r x] -- a bit hacky maybe
      quote s = '"':(s ++ ['"'])

