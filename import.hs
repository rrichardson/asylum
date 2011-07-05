{-# LANGUAGE OverloadedStrings #-}

import qualified Text.XML.Enumerator.Parse as X
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.BZip as BZip
import Data.Text.Lazy (Text, unpack)

--- based off http://wikimedia.org/xml/export-0.4.xsd

data Page = Page { page_content :: Text, page_id :: Int } deriving Show

parseMediawiki = X.tagNoAttr "mediawiki" $ X.many parsePage

parsePage = X.tagNoAttr "page" $ do
    pg1 <- X.choose [parseRevision, parseUpload, parseLogitem]
    pg2 <- parseId
    return $ case pg1 of
        Nothing -> Nothing
        Just (Page txt _) -> case pg2 of 
                                Just (Page _ pid) -> Just $ Page txt pid
                                Nothing -> Nothing

parseText = X.tagNoAttr "text" $ do 
    txt <- X.content
    return $ Page { page_content = txt } 

parseId = X.tagNoAttr "id" $ do
    pid <- X.content
    return $ Page { page_id = (read $ unpack pid) } 


parseRevision = X.tagNoAttr "revision" $ parseText

parseLogitem = X.tagNoAttr "logitem" $ parseText

parseUpload = X.tagNoAttr "upload" $ return $ Page { page_content = "" } 

main = do
  content <- fmap BZip.decompress (BS.readFile "./data/enwiki-latest-pages-articles.xml.bz2")
  pages <- X.parseBytes X.content X.decodeEntities $ X.force "mediawiki required" parseMediawiki
  print $ take 10 pages
