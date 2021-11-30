{-# LANGUAGE FlexibleContexts #-}

module Find where

import Elem.Types (ElementRep, GroupHtml(GroupHtml), Elem, mkGH, Elem', TreeHTML, ShowHTML)
-- import Elem.TreeElemParser (findSameTreeH)
import Types (ScrapeFail(..))

import Text.Parsec (ParsecT, ParseError, Parsec, Stream, parse, eof, anyChar, (<|>), try, parserZero) 
import Data.Text (Text)
import Data.Functor.Identity (Identity)
import Data.Either (fromRight)
-- | This module provides an interface for getting patterns seperated by whatever in a given source
-- | that you plan to parse

-- | findSequential(_x) is for information rich elements such as products that should have multiple fields
-- | that the user would like to return 




-- | Converts a parsing/scraping pattern to one which either returns Nothing
-- | or Just a list of at least 1 element. Maybe type is used so that there is a clearer
-- | distinction between a failed search and a successful one
findNaive :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Maybe [a])
findNaive p = (justify .  (fromRight mempty) . sequenceA) <$> (find p)
  where
    justify x = if length x == 0 then Nothing else Just x 



-- givesNothing :: ParsecT e s m (Either ScrapeFail a) 
-- givesNothing = Left NonMatch <$ anyChar

findSequential :: Stream s m Char => [ParsecT s u m a] -> ParsecT s u m [Either ScrapeFail a] 
findSequential parsers = undefined -- builds off findUntilMatch

findSequential2 :: Stream s m Char => (ParsecT s u m a, ParsecT s u m b) -> ParsecT s u m (a,b)
findSequential2 (a,b) = do
  a' <- findUntilMatch a
  b' <- findUntilMatch b
  return (a', b')

findSequential3 :: Stream s m Char => (ParsecT s u m a, ParsecT s u m b, ParsecT s u m c) -> ParsecT s u m (a,b,c)
findSequential3 (a,b,c) = do
  a' <- findUntilMatch a
  
  b' <- findUntilMatch b
  c' <- findUntilMatch c
  return (a', b', c')

-- | Like find naive except that finishes parsing on the first match it finds in the document
findUntilMatch :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
findUntilMatch parser = do
  x <- (try (baseParser parser)) <|> givesNothing
  case x of
    Right a -> return a
    Left NonMatch -> findUntilMatch parser 
    Left Eof -> parserZero


-- -- this is for sequencing matches amongst noise
-- findUntilMatch2 :: ParsecT s u m a -> ParsecT s u m (Either ScrapeFail a)
-- findUntilMatch2 parser = do
--   x <- (try (baseParser parser)) <|> givesNothing
--   case x of
--     Right a -> return $ Right a
--     Left NonMatch -> findUntilMatch parser 
--     Left Eof -> parserZero 
  


      
-- -- Note: List will be backwards as is 
find :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [Either ScrapeFail a]
find parser = do
  x <- (try (baseParser parser)) <|> givesNothing <|> endStream
  case x of
    Right a -> fmap (x :) (find parser)
    Left Eof -> return []
    Left NonMatch -> find parser
-- return (x:xs)





-- findSome = undefined
-- findSomeSame = findSomeSameEl



baseParser :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Either ScrapeFail a)
baseParser parser = fmap Right parser

givesNothing :: Stream s m Char => ParsecT s u m (Either ScrapeFail a) 
givesNothing = Left NonMatch <$ anyChar

endStream :: (Stream s m t, Show t) => ParsecT s u m (Either ScrapeFail a)
endStream = try (eof) >> (return $ Left Eof)




-- | Just since do we really care about non matches?
findSomeHTMLNaive :: Stream s Identity Char => Parsec s () a -> s -> (Maybe [a])
findSomeHTMLNaive parser text =
  let parser' = findNaive parser  
  in 
    case parse parser' "from html:add-in URL soon" text of
      Left _ -> Nothing 
      Right maybe_A -> maybe_A

findSomeHTML :: Stream s Identity Char => Parsec s () a -> s -> Either ParseError (Maybe [a])
findSomeHTML parser text =
  let parser' = findNaive parser  
  in parse parser' "from html at this url: <unimplemented - derp>" text

-- findFirst :: ParsecT s u m a -> Text -> Maybe a 
-- findFirst = undefined

-- findAllHtml :: ParsecT s u m a -> Text -> Maybe a 
-- findAllHtml = undefined
-- | My findAll' function design / runParserOnHtml 
  --use Maybe instead of Either to toss failure
  --case [] -> Nothing

-- | so it returns :: Maybe [a] = Just [a] | Nothing
  -- which will be beautiful for modeling at high level from scrape result to scrape result

-- | I also really need to implement non-zero, non-ending predicate inner function
-- | like nonZeroSep https://hackage.haskell.org/package/replace-megaparsec-1.4.4.0/docs/src/Replace.Megaparsec.html#sepCap

-- | NOTE: I can replace manyTill_ with anyTill from Replace.Megaparsec


-- within :: m a -> m a -> m a
-- within ma mb = do
--   x <- do
--     ma 
--     y <- mb
    
--     return mb 





-- -- Mutually exclusive/non-overlapping patterns 
-- findAll' :: ParsecT s u m a -> ParsecT s u m [a]
-- findAll' parser = do
--   x <- skipManyTill anyChar parser <|> return []
--   xs <- findAll' parser
--   return (x : xs)


    
        
findAllBetween = undefined



-- | Use with constructed for parsing datatype 
buildSequentialElemsParser :: ParsecT s u m [a]
buildSequentialElemsParser = undefined
-- | to be applied to inner text of listlike elem


findOnChangeInput :: ParsecT s u m (Elem' a)
findOnChangeInput = undefined
-- eg : <select id="s-lg-sel-subjects" name="s-lg-sel-subjects" class="form-control" data-placeholder="All Subjects" onchange="springSpace.publicObj.filterAzBySubject(jQuery(this).val(), 3848);">


-- | Rewrite to being any pattern "a"

-- -- | Note: this isnt necessarily deprecated but just useful for when we want to find many of some pattern
-- -- | that doesnt need to exist right after the previous successful match
-- {-# DEPRECATED findSomeSameEl "need manytill out and useful for find, findAll" #-}
-- findSomeSameEl :: (Stream s m Char, ShowHTML a)
--                => Maybe (ParsecT s u m a)
--                -> Maybe [Elem]
--                -> [(String, Maybe String)]
--                -> ParsecT s u m [TreeHTML a]
-- findSomeSameEl matchh elemOpts attrsSubset = do
--   -- (_, treeH) <- manyTill_ (anyChar) (try $ treeElemParser elemOpts matchh attrsSubset)
--   treeH <- treeElemParser elemOpts matchh attrsSubset
--   treeHs <- findMore matchh treeH
--   case treeHs of
--     [] -> parserFail "no matches" -- by definition: this func should return at least 1 copy 
--     _ -> return (treeH : treeHs)
--   where
--     findMore :: (Stream s m Char, ShowHTML a) =>
--                 Maybe (ParsecT s u m a)
--              -> TreeHTML a
--              -> ParsecT s u m [TreeHTML a]    
--     findMore matchh treeH = do
--       treeH' <- --( fmap (:[]) (skipManyTill anyChar (try $ findSameTreeH matchh treeH) )  )
--                 (do
--                     -- note: using skipManyTill VIOLATES expectations of this functions use
--                     -- this is gonna return something like 19 <a></a> tags since it is not
--                     -- in any way required for the congruent elements to be neighbours 
                    
--                   x <- skipManyTill anyChar (try $ findSameTreeH matchh treeH)
--                   return (x:[])
--                 )
--                 <|> return []
--       case treeH' of
--         [] -> return []
--         _ -> fmap ((treeH:[]) <>) $ findMore matchh treeH -- TreeHTML : ParsecT s u m [TreeHTML]
