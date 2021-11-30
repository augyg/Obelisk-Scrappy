{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Elem.ChainHTML where


import Elem.Types (Elem', ShowHTML, innerText')
import Find (findNaive)
import Links (maybeUsefulUrl)
import Elem.ElemHeadParse (hrefParser)

import Text.Parsec (ParsecT, Stream, char, (<|>), many, parserFail, parse, parserZero, string)
import Control.Applicative (some, liftA2)
-- import Text.Megaparsec (manyTill_)

import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
-- functions for chaining free-range html patterns based on the previous
-- patterns to allow for maximum flexibility 


manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go



clean :: String -> String
clean = undefined -- drop if == ( \n | \" | '\\' )


-- same site is guranteed
allLinks :: String -> ParsecT String () Identity [String] 
allLinks baseUrl = do
  x <- findNaive hrefParser 
  return $ case x of
    Just (x':xs') -> catMaybes $ fmap (maybeUsefulUrl baseUrl) (x':xs')
    Just [] -> []
    Nothing -> [] 



mustContain :: ParsecT s u m (Elem' a) -> Int -> ParsecT s u m b -> ParsecT s u m (Elem' a)
mustContain e count pat = do
  out <- e
  case parse (findNaive $ string "Search") "" (innerText' out) of
    Right (Just xs) -> if count > (length xs) then parserZero else return out
    _ -> parserZero
    

-- I could always make this generalized to a stream by making
  -- Stream s => .. data Elem2' = Elem2' s  ...
contains :: ParsecT s u m (Elem' a) -> ParsecT String () Identity b -> ParsecT s u m b
contains a b = do
  x <- a

  let
    ridNL p = (many (char ' ' <|> char '\n')) >> p 
  
  -- need to skip 
  case parse (ridNL b) "" (innerText' x) of
    Right match -> return match
    Left err -> parserFail (show err)

-- finds multiple matches anywhere inside the passed elem
contains' :: ShowHTML a =>
             ParsecT s u m (Elem' a) 
          -> ParsecT String () Identity b
          -> ParsecT s u m (Maybe [b])
contains' a b = do
  x <- a
  case parse (findNaive b) "" (innerText' x) of
    Right match -> return match
    Left err -> parserFail (show err)




sequenceHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
sequenceHtml p1 p2 = do
  x <- p1
  _ <- many (char ' ' <|> char '\n')
  y <- p2
  return (x, y)

sequenceHtml_ :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b
sequenceHtml_ p1 p2 = do
  _ <- p1
  _ <- many (char ' ' <|> char '\n')
  p2

(</>>) :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b 
(</>>) = sequenceHtml_

(</>>=) :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
(</>>=) = sequenceHtml

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


manyHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
manyHtml prsrHtml = (many (char ' ' <|> char '\n')) >> many (fmap snd $ manyTill_ (char ' ' <|> char '\n') prsrHtml)

someHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
someHtml prsrHtml = many (char ' ' <|> char '\n') >> some prsrHtml



