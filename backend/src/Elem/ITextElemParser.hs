{-# LANGUAGE FlexibleContexts #-}

module Elem.ITextElemParser where

import Elem.ElemHeadParse (parseOpeningTag, buildElemsOpts)
import Elem.Types (HTMLMatcher(..), endTag, selfClosingTextful)

import Text.Megaparsec as MParsec (some)
import Text.Parsec (parse, ParsecT, Stream, string, (<|>), anyChar, char, optional, try, manyTill, many, runParserT, ParseError)
import Control.Monad
import Data.Either (fromRight)
-- justPlaintext :: Int -> ParsecT s u m [String]
-- justPlaintext atLeast = 
--   fmap (filter (\x -> length x > atLeast)) {--} (find $ many (letter <|> number <|> char ' ' <|> char '.'))
  



-- | Note: will need more complex accumulator for case where an elem has two distinct text segements broken up
-- | by an element, (rare case)


type Html = String
-- getPlainText :: Html -> Either ParseError [String]
-- getPlainText html = do
  -- let
    -- expr = (fmap show $ parseOpeningTag (Just styleTags) [])
            -- <|> (string "</" >> buildElemsOpts styleTags >> string ">")
    -- styleTags = ["b", "strong", "i", "em", "mark", "small", "ins", "sub", "sup"]   --"del" omitted
     
  -- divied <- parse (divideUp expr) "" html    
  -- parse onlyPlainText "" $ (mconcat . catEithers) divied

removeStyleTags :: Html -> Html
removeStyleTags html = (mconcat . catEithers) $ fromRight undefined $ parse (divideUp expr) "" html
  where expr = (fmap show $ parseOpeningTag (Just styleTags) [])
               <|> (string "</" >> buildElemsOpts styleTags >> string ">")
        styleTags =  ["b", "strong", "i", "em", "mark", "small", "ins", "sub", "sup"] 
  
-- getPlainText' :: ParsecT s u m [String]
-- getPlainText' = do
  

  -- join $ fmap (  (parse onlyPlainText "")  . mconcat . catEithers) $ 
  -- where
    -- expr = (fmap show $ parseOpeningTag (Just styleTags) [])
            -- <|> (string "</" >> buildElemsOpts styleTags >> string ">")
    -- styleTags = ["b", "strong", "i", "em", "mark", "small", "ins", "sub", "sup"]   --"del" omitted 

-- Just applies onlyPlainText to html tag
-- getDocText :: Html -> [String]
-- getDocText html = 


catEithers :: [Either e a] -> [a]
catEithers (x:xs) = case x of
  Right a -> a : catEithers xs
  Left _ -> catEithers xs
  -- in this case, our Right case are the ones we want to eliminate

divideUp :: Stream s m Char => ParsecT s u m String -> ParsecT s u m [Either String String]
divideUp parser = many ((Right <$> parser) <|> ( (Left . (:[]) ) <$> anyChar)) 

onlyPlainText :: Stream s m Char => ParsecT s u m String
onlyPlainText = fmap (\(ACT strings) -> mconcat strings) specialElemParser 
  where
    specialElemParser :: Stream s m Char => ParsecT s u m (AccumITextElem String)
    specialElemParser = do
      (elem', attrs') <- parseOpeningTag (Just ["html"]) []  
      (localText, inTex) <- fmap (foldr textOnlyFoldr mempty)
                                $ (try (string "/>") >> return [])
                                <|> (try $ innerElemParser' elem')
                                <|> (selfClosingTextful Nothing) -- did not have an easily associated end tag
      return $ ACT (localText : inTex) 
      
      where innerElemParser' eTag = --htmlGenParser with specialElemParser 
              char '>'
              >> manyTill (Element <$> (try specialElemParser) 
                           <|> ((IText . (:[])) <$> anyChar)  ) (endTag eTag)

            -- selfClosingTextful = manyTill (IText . (:[]) <$> anyChar) endTagg
            -- endTagg = (try (char '<'
                            -- >> (optional (char '/'))
                            -- >> MParsec.some anyChar
                            -- >> (string " " <|> string ">")))

-- Not for getting matches 
data AccumITextElem a = ACT [String]

textOnlyFoldr :: HTMLMatcher AccumITextElem String -> (String, [String]) -> (String, [String]) 
textOnlyFoldr htmlM (itextAccum, fromElemAccum) = case htmlM of 
  IText str -> 
    (itextAccum <> str, fromElemAccum) 
  Element (ACT strList) ->
    (itextAccum, fromElemAccum <> strList)
  -- should never fire
  Match mat ->
    (itextAccum <> mat, fromElemAccum)

-- Should consider using the following data structure




-- textOnlyFoldr :: [HTMLMatcher e a] -> [String] 
-- textOnlyFoldr htmlMs = fmap f htmlMs -- . filter (\x -> case x of { IText x -> True; _ -> False }) htmlMs
--       where
--         f htmlM = case htmlM of 
--                     Match str -> str
--                     IText str -> str
--                     Element e  -> innerText' e
                      
