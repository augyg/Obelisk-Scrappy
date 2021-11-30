{-# LANGUAGE FlexibleContexts #-}

module Requests where 

import Find (findNaive)
import Elem.SimpleElemParser (el)
import Elem.Types (innerText')
import Elem.ChainHTML (contains)

import Data.List (isInfixOf)
import Network.HTTP.Types.Header 
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT, ParseError, parse, Stream, many)
import Network.HTTP.Client (Manager, Proxy(..), HttpException, httpLbs, responseBody, parseRequest
                           , secure, requestHeaders, newManager, useProxy, managerSetSecureProxy)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Exception (catch)

type Link = String
type ParsecError = ParseError


-- Applied inside of execState                          ---Goal---IO (Either ParsecError (Maybe a))
scrapeUrlWith :: ParsecT Text () Identity a -> Manager -> Link -> IO (Either ParsecError a)
scrapeUrlWith parser manager url = do
  --replace with successive requesting with cookies
  -- let url' = evalLink url
  request <- parseRequest url
  response <- httpLbs request manager
  let
    dadBod = toStrict $ decodeUtf8 (responseBody response)
  -- response <- (decodeUtf8 (responseBody response)) <$> httpLbs request manager

  return $ parse parser ("site" <> url) dadBod


type Html = String






-- | Get html with no Proxy 
getHtml' :: String -> IO String
getHtml' url = do
  mgrHttps <- newManager tlsManagerSettings
  requ <- parseRequest url
  response <- httpLbs requ mgrHttps
  let
    dadBod = (unpack . toStrict . decodeUtf8 . responseBody) response

  return dadBod


-- | Gurantees retrieval of Html by replacing the proxy if we are blocked or the proxy fails 
getHtml :: Manager -> String -> IO (Manager, Html)
getHtml manager url = do
  -- mgrHttps <- newManager tlsManagerSettings
  requ <- parseRequest url
  let
    headers = [ (hUserAgent, "Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0")
              -- , (hAccept, "image/webp, */*")
              , (hAcceptLanguage, "en-US,en;q=0.5")
              , (hAcceptEncoding, "gzip, deflate, br")
              , (hConnection, "keep-alive")
              -- , (hReferer, "https://www.amazon.ca/")
              -- , (hTE, "Trailers")
              ]
    req = requ { requestHeaders = (fmap . fmap) (encodeUtf8 . pack) headers
               , secure = True
               }

    dadBod response = (unpack . toStrict . decodeUtf8 . responseBody) response
    
    f = do
      res <- httpLbs requ manager
      return (manager, dadBod res)

    g :: HttpException -> IO (Manager, String)
    g = (\_ -> do
            newManager <- mkManager
            getHtml newManager url
        )
  (manager', response) <- catch f g 
  return (manager', response)



testForValidProxy :: [Proxy] -> IO (Manager)
testForValidProxy (proxy:proxies) = do
  req <- parseRequest "https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:catch"
  trialManager <- mkManagerInternal proxy
  print $ "hello"
  let
    f :: IO (Manager)
    f = catch (httpLbs req trialManager >> return trialManager) g

    g :: HttpException -> IO (Manager)
    g = (\_ -> testForValidProxy proxies) 
  x <- f
  return x



rowToProxy :: [String] -> Proxy
rowToProxy row = Proxy ((encodeUtf8 . pack) (row !! 0)) (read (row !! 1) :: Int)

mkManagerInternal :: Proxy -> IO Manager
mkManagerInternal proxy = newManager (managerSetSecureProxy (useProxy proxy) tlsManagerSettings)

  

mkManager :: IO (Manager)
mkManager = do
  proxyRows <- scrapeProxyList
  testForValidProxy proxyRows




scrapeProxyList :: IO [Proxy] -- becoming [[String]]
scrapeProxyList = do
  response <- getHtml' "https://free-proxy-list.net/"
  let
    parser = el "tr" [] `contains` b
    b :: Stream s m Char => ParsecT s u m [String]
    b = ((fmap . fmap) innerText' $ many (el "td" []))

    bePicky :: [[String]] -> [[String]]
    bePicky rows = filter (\x -> not $ (isInfixOf "anonymous" (x !! 4)) || (isInfixOf "elite proxy" (x !! 4))) rows
    
    -- g :: [String] -> Proxy
    -- g row = Proxy ((encodeUtf8 . pack) (row !! 0)) (read (row !! 1) :: Int)
    
  -- mapM_ print $ fromMaybe [] $ fromRight Nothing (parse (findNaive parser) "" response)
    
  case parse (findNaive parser) "" response of
    Right (Just (rows)) -> --f rows   --row is a list of 9ish "td" (cell:rowCells)
      return (fmap rowToProxy (rows))
      --  $ (\row -> Proxy (row !! 0) (row !! 1))
    Right (Nothing) -> ioError $ userError "proxy error: couldnt get proxy"
    Left err -> ioError $ userError (show err)
    



  

-- mkProxy :: Proxy 
-- mkProxy = Proxy { proxyHost = fst fromScrapeProxies
--                 , proxyPort = snd fromSrapeProxies  
--                 }
type Host = String
type Port = String
