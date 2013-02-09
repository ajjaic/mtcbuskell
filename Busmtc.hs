module Busmtc (
  getBusList
, getBusDetails
, busListUrl
) where

import Network.HTTP
import Text.HTML.TagSoup
import Text.StringLike (StringLike)

busListUrl = "http://www.mtcbus.org/Routes.asp"
prebus = "http://www.mtcbus.org/Routes.asp?cboRouteCode="
postbus = "&submit=Search"

getBusDetails :: String -> IO ([String], [String])
getBusDetails b = do 
  a <- getHtmlPage $ constructurl b
  return $ exBusDetails $ getStartEndSection "<Tr BGColor='#EAEAEA'>" "<A Href='#' OnClick='history.back();'>" a

-- |Extract the information about the particular bus given as a parameter
exBusDetails :: Show a => [Tag a] -> ([a], [a])
exBusDetails t = (busd, busp) where
  a = filter isTagText t
  busd = map fromTagText $ take 5 a
  busp = alternate $ init $ map fromTagText $ drop 7 a
  alternate (x:y:xs) = y:(alternate xs)
  alternate _ = []

getBusList :: String -> IO [String]
getBusList url = do
  a <- getHtmlPage url
  return $ exBusList $ init $ getStartEndSection "<Option>" "</Select>" a

-- |Extract the list of buses
exBusList :: Show a => [Tag a] -> [a]
exBusList ts = foldl helpa [] ts where 
  helpa acc t = if isTagText t
                          then (fromTagText t):acc
                          else acc

constructurl :: String -> String
constructurl b = concat [prebus, b, postbus]

{-             Helper Functions             -}

-- |Gets the source of a remote html page
getHtmlPage :: String -> IO String
getHtmlPage url = resp 
          >>= (\r -> getResponseBody r)
          >>= (\b -> return b) 
  where
    resp = simpleHTTP req
    req = getRequest url
    
-- |Given a starting string, ending string and the html string, the function returns all the tags, attributes and text within the starting and
-- ending tags
getStartEndSection  :: (StringLike str, TagRep t) => t -> t -> str -> [Tag str]
getStartEndSection ss es s = tillend where
  parsed = parseTags s
  fromstart = head $ sections (~== ss) parsed
  tillend = fst $ break (~== es) fromstart

saveHtmlPage :: String -> String -> IO ()
saveHtmlPage fn url = getHtmlPage url
              >>= (\s -> writeFile fn s) 
