{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative (optional)
import qualified Text.Show.Pretty as PP
import qualified Text.XML.Light as XML
import qualified Text.XML.Xleb as X

data Feed = Feed
  { feedTitle    :: String
  , feedSubtitle :: String
  , feedLinks    :: [Link]
  , feedId       :: String
  , feedUpdated  :: String
  , feedEntries  :: [Entry]
  } deriving (Show)

data Link = Link
  { linkHref :: String
  , linkRel  :: Maybe String
  } deriving (Show)

data Entry = Entry
  { entryTitle   :: String
  , entryLinks   :: [Link]
  , entryId      :: String
  , entryUpdated :: String
  , entrySummary :: String
  , entryAuthor  :: Author
  , entryContent :: Content
  } deriving (Show)

data Content
  = XHTMLContent XML.Element
  | HTMLContent String
  | TextContent String
    deriving (Show)

data Author = Author
  { authorName  :: String
  , authorEmail :: String
  } deriving (Show)

feed :: X.Xleb Feed
feed = X.elem "feed" $ do
  feedTitle    <- X.child "title" (X.contents X.string)
  feedSubtitle <- X.child "subtitle" (X.contents X.string)
  feedLinks    <- X.children "link" link
  feedId       <- X.child "id" (X.contents X.string)
  feedUpdated  <- X.child "updated" (X.contents X.string)
  feedEntries  <- X.children "entry" entry
  return Feed { .. }

link :: X.Xleb Link
link =
  Link <$> X.attr "href" X.string
       <*> optional (X.attr "rel" X.string)

entry :: X.Xleb Entry
entry = X.elem "entry" $ do
  entryTitle    <- X.child "title" (X.contents X.string)
  entryLinks    <- X.children "link" link
  entryId       <- X.child "id" (X.contents X.string)
  entryUpdated  <- X.child "updated" (X.contents X.string)
  entrySummary  <- X.child "summary" (X.contents X.string)
  entryAuthor   <- X.child "author" author
  entryContent  <- X.child "content" content
  return Entry { .. }

content :: X.Xleb Content
content = do
  typ <- X.attr "type" X.string
  case typ of
    "xhtml" -> XHTMLContent <$> X.rawElement
    "html"  -> HTMLContent <$> X.contents X.string
    "text"  -> TextContent <$> X.contents X.string
    _       -> fail "Unknown content type"

author :: X.Xleb Author
author =
  Author <$> X.child "name" (X.contents X.string)
         <*> X.child "email" (X.contents X.string)

main :: IO ()
main = do
  cs <- getContents
  PP.pPrint (X.runXleb cs feed)
