{-# LANGUAGE LambdaCase, GADTs, OverloadedStrings, FlexibleContexts #-}


module Backend where

import Snap.Core
import Data.Maybe (fromJust)

import Scrape (runScraperOnHtml)
import Requests (getHtml')
import Elem.SimpleElemParser (el)
import Elem.ElemHeadParse (parseOpeningTagF)
import Elem.Types (ElemHead)

import qualified Data.Map as Map 
import qualified Data.Text as T
import Text.Parsec (ParsecT, Stream, parserZero)

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Control.Monad.IO.Class

type HrefURI = String 

-- | Written like this in order to discard memory as soon as its not needed
hrefScraper :: Stream s m Char => ParsecT s u m HrefURI
hrefScraper = do
  -- we parse more than we need to in order to ensure that we match the correct pattern of an href
  (_, attributes) <- parseOpeningTagF "class" (=="yWs4tf")
  case Map.lookup "src" attributes of
    Just href -> return href
    Nothing -> parserZero
      -- a fail of runScraperOnHtml only tells us Nothing anyways ; which we know just means
      -- no patterns found

sowwyIveFailedYou :: T.Text
sowwyIveFailedYou = "https://www.google.com/url?sa=i&url=http%3A%2F%2Fwww.quickmeme.com%2Fmeme%2F3ti4m2&psig=AOvVaw3_K5T9jQe2PC7cnLIPHTpz&ust=1638325178152000&source=images&cd=vfe&ved=0CAsQjRxqFwoTCMiplIWDv_QCFQAAAAAdAAAAABAD"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run =
      \serve -> serve $ \case 
        BackendRoute_Main :/ word -> do
          liftIO $ print word
          html <- liftIO $ getHtml' (mkGoogleUrl . T.unpack $ (word :: T.Text))
          -- liftIO $ mapM_ print $ fromJust $ runScraperOnHtml (el "img" []) html
          -- liftIO $ print $ runScraperOnHtml (parseOpeningTagF "class" (=="rg_i Q4LuWd")) html
          case runScraperOnHtml hrefScraper html of
            Just (href:_) -> writeText . T.pack $ href
            _ -> writeText "nope not yet" --sowwyIveFailedYou
        BackendRoute_Missing :/ () -> error "404"
          -- ()
          -- mainB word 
  , _backend_routeEncoder = fullRouteEncoder
  }


-- If you wanted to do further analysis on the scraped pattern you could use Maybe's monadic interface
-- = do
  -- list <- runScraperOnHtml prsr html
  -- return $ f list 

--serve $ const $ return ()
-- mainB word = 

mkGoogleUrl :: String -> String
mkGoogleUrl t = 
  "https://www.google.com/search?q=" <> t <> "&sxsrf=AOaemvJc28k4ka_rY6sAp-TapWE8ldF4iA:1638157797795&source=lnms&tbm=isch&sa=X&ved=2ahUKEwj24_2s1bz0AhVJnGoFHX--AuwQ_AUoAXoECAEQAw&biw=1920&bih=979&dpr=1" 

-- "https://www.google.com/search?q=Fajitas&sxsrf=AOaemvJc28k4ka_rY6sAp-TapWE8ldF4iA:1638157797795&source=lnms&tbm=isch&sa=X&ved=2ahUKEwj24_2s1bz0AhVJnGoFHX--AuwQ_AUoAXoECAEQAw&biw=1920&bih=979&dpr=1" 

type HrefURL = String 

getHref' :: ElemHead -> Maybe HrefURL
getHref' (_, as) = Map.lookup "href" as 
