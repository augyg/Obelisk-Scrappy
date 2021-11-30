{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

-- import FrontendTwo
import Scrape (runScraperOnHtml)
import Requests (getHtml')
import Elem.ElemHeadParse (hrefParser)

import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.IO.Class
import qualified Data.Map as Map 

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM, MonadJSM)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = prerender_ blank $ do
      (newSearchTerm, buttonClick) <- el "div" $ do 
        newSearchTerm <- el "div" $ fmap value (inputElement def)
        buttonClick <- button "scrape away"
        return (newSearchTerm, buttonClick)
      response <- performRequestAsync $ buildReq (tag (current newSearchTerm) buttonClick)
      let 
        dadBod = fmap ((fromMaybe "not sure what happened") . _xhrResponse_responseText) response 
      primeDadBod <- holdDyn startingImage dadBod
      -- dynText primeDadBod
      el "div" $ elDynAttr "img" (fmap funcShui primeDadBod) $ blank
      
      return ()
  }

type HrefURI = T.Text

funcShui :: HrefURI -> Map.Map T.Text T.Text
funcShui href = "src" =: href
                <> "width" =: "300px"
                <> "height" =: "auto"

buildReq :: Reflex t => Event t T.Text -> Event t (XhrRequest ())
buildReq = fmap (\texy -> XhrRequest "GET" (termToUrl texy) def)
  

termToUrl :: T.Text -> T.Text
termToUrl term = "/backend/" <> term

startingImage :: T.Text
startingImage = "https://aceinterviewprep.s3.ca-central-1.amazonaws.com/static/images/c88a73d8b1a44d5782dd5c2e0bfd13d0.png"

mkGoogleUrl t = "https://www.google.com/"
  -- "https://www.google.com/search?q=" <> t <> "&sxsrf=AOaemvJc28k4ka_rY6sAp-TapWE8ldF4iA:1638157797795&source=lnms&tbm=isch&sa=X&ved=2ahUKEwj24_2s1bz0AhVJnGoFHX--AuwQ_AUoAXoECAEQAw&biw=1920&bih=979&dpr=1" 













-- -- funcyAf :: (DomBuilder t m
--            -- , MonadJSM (Performable m)
--            -- , HasJSContext (Performable m)
--            -- , PerformEvent t m
--            -- , TriggerEvent t m
--            -- , MonadIO (Performable m)) => m ()
-- funcyAf :: (DomBuilder t m
--            , PostBuild t m
--            , MonadHold t m
--            , PerformEvent t m
--            , TriggerEvent t m
--            , TriggerEvent t (Client m)
--            , PerformEvent t (Client m)
--            , HasJSContext (Performable (Client m))
--            , MonadJSM (Performable (Client m))
--            , MonadIO (Client m)
--            , MonadJSM (Performable m)
--            , Prerender js t m
--            , MonadIO m
--            , HasJSContext (Performable m)) => m ()
-- -- funcyAf :: DomBuilder t m => m ()
-- funcyAf = do
--   newSearchTerm <- el "div" $ do
--     inpElem <- inputElement def
--     let
--       ourSearchTerm = _inputElement_value inpElem
--       scraper = runScraperOnHtml hrefParser
--     return $ (current ourSearchTerm)
--   buttonClick <- button "scrape away"
--   response <- getAndDecode (tag newSearchTerm buttonClick)
--   -- txt <- holdDyn "" (fmap fromJust response)
--   let
--     -- txt :: FromJSON => Event t (Maybe a)
--     -- txt = fmap fromJust $ switch (current response)
--   maybeNewVal <- holdDyn (Just "nothing" :: Maybe T.Text) response
--   dynText $ fmap (fromMaybe "Nothinggg") maybeNewVal
--   -- let
--     -- dadBod :: Event t XhrResponseBody
--     -- dadBod = fmap (fromJust . _xhrResponse_response) response
--     -- f (XhrResponseBody_Default txt) = T.unpack txt 
                                      
--   return ()
