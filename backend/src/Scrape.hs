{-# LANGUAGE FlexibleContexts #-}

module Scrape where

-- Basically just html patterns from testing / courtney market stuff
import Elem.Types (Elem', innerText')
import Elem.ElemHeadParse (hrefParser, parseOpeningTag)
import Elem.SimpleElemParser (el)
import Elem.ChainHTML ((</>>))

import Find (findNaive)
import Links (maybeUsefulUrl)

import Data.Either (fromRight)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Functor.Identity (Identity)
import Text.Parsec (Stream, ParsecT, parse, anyChar, manyTill, char)


-- | Find all occurences of a given parsing/scraping pattern
-- | e.g. getHtml' "https://google.ca" >>= return . runScraperOnHtml (el "a" []) , would give all 'a' tag html elements on google.ca  
runScraperOnHtml :: ParsecT String () Identity a -> String -> Maybe [a]
runScraperOnHtml p html = fromRight Nothing $ parse (findNaive $ p) "" html 


runScraperInBody :: ParsecT String () Identity a -> String -> Maybe [a]
runScraperInBody prsr html = fromRight Nothing $ parse (skipToInBody >> findNaive prsr) "" html

skipToInBody :: Stream s m Char => ParsecT s u m ()
skipToInBody = manyTill anyChar (parseOpeningTag (Just ["html"]) [] >> char '>')
               </>> el "head" []
               </>> parseOpeningTag (Just ["body"]) []
               >> char '>'
               >> return () 

  
runScraperOnBody :: ParsecT String () Identity a -> String -> Maybe [a] 
runScraperOnBody prsr html = fromRight Nothing $ parse (skipToBody >> findNaive prsr) "" html 

skipToBody :: Stream s m Char => ParsecT s u m ()
skipToBody = manyTill anyChar (parseOpeningTag (Just ["html"]) [] >> char '>') </>> el "head" [] >> return () 


runScraperOnHtml1 :: ParsecT String () Identity a -> String -> Maybe a
runScraperOnHtml1 p = (fmap head) . runScraperOnHtml p


-- {-# DEPRECATED simpleScrape' "from fba project - gives confusing String output" #-}
-- simpleScrape' :: ParsecT String () Identity String -> String -> String 
-- simpleScrape' p html = case parse (findNaive p) "" html of
--   Right (Just (x:_)) -> x
--   Right (Just []) -> "NothingA"
--   Right (Nothing) -> "NothingB"
--   Left err -> "Nothing" <> show err



-- clean :: String -> String
-- clean = undefined -- drop if == ( \n | \" | '\\' )


-- | uses maybeUsefulUrl to get all links on page pointing only to same site links
allLinks :: String -> ParsecT String () Identity [String] 
allLinks baseUrl = do
  x <- findNaive hrefParser 
  return $ case x of
    Just (x':xs') -> catMaybes $ fmap (maybeUsefulUrl baseUrl) (x':xs')
    Just [] -> []
    Nothing -> [] 

type Name = String -- placeholder
tableItem :: Name -> Elem' String
tableItem = undefined




-- scrapeInnerText :: ParsecT String () Identity (Elem' String) -> String 
-- scrapeInnerText p = case parse (findNaive p) "" body of
--   Right (Just (x:_)) -> innerText' x
--   Right (Just []) -> "Nothing"
--   Right (Nothing) -> "Nothing"
--   Left err -> show err




scrapeFirst :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Maybe a)
scrapeFirst p = do
  x <- findNaive p
  case x of
    Just (x:_) -> return $ Just x
    Nothing -> return $ Nothing

 
findCount :: Stream s m Char => ParsecT s u m a -> ParsecT s u m Int
findCount p = do
  x <- findNaive p
  return $ length (fromMaybe [] x)

