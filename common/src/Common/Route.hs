{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Main :: BackendRoute Text -- This must match the result of the encoder
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Main -> PathSegment "backend" $ singlePathSegmentEncoder--(unitEncoder mempty)
  )
  -- pathParamEncoder singlePathSegmentEncoder
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- mkFullRouteEncoder
--   :: (GCompare br, GCompare fr, GShow br, GShow fr, UniverseSome br, UniverseSome fr)
--   => R (FullRoute br fr) -- ^ 404 handler
--   -> (forall a. br a -> SegmentResult (Either Text) (Either Text) a) -- ^ How to encode a single backend route segment
--   -> (forall a. fr a -> SegmentResult (Either Text) (Either Text) a) -- ^ How to encode a single frontend route segment
--   -> Encoder (Either Text) Identity (R (FullRoute br fr)) PageName
-- mkFullRouteEncoder missing backendSegment frontendSegment = handleEncoder (const missing) $
--   pathComponentEncoder $ \case
--     FullRoute_Backend backendRoute -> backendSegment backendRoute
--     FullRoute_Frontend obeliskRoute -> obeliskRouteSegment obeliskRoute frontendSegment


-- data SegmentResult check parse a =
--     PathEnd (Encoder check parse a (Map Text (Maybe Text))) -- ^ Indicate that the path is finished, with an Encoder that translates the corresponding value into query parameters
--   | PathSegment Text (Encoder check parse a PageName) -- ^ Indicate that the key should be represented by an additional path segment with the given 'Text', and give an Encoder for translating the corresponding value into the remainder of the route.
