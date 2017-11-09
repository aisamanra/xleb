{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Text.XML.Xleb
Description : The Xleb XML-parsing monad
Copyright   : (c) Getty Ritter, 2017
License     : BSD
Maintainer  : Getty Ritter <xleb@infinitenegativeutility.com>
Stability   : experimental

The 'Xleb' monad (and the corresponding 'XlebT' monad transformer) is
a monadic sublanguage for easily parsing XML structures.

This module is intended to be imported qualified, to avoid name
clashes with 'Prelude' functions. e.g.

> import qualified Text.XML.Xleb as X

-}


module Text.XML.Xleb
( -- * How To Use 'Xleb'
  -- $use

  -- * The 'Xleb' monad
  Xleb
, runXleb
  -- ** The 'XlebT' monad transformer
, XlebT
, runXlebT
-- * Errors
, XlebError(..)
, errorString
-- * Element Structure
, elem
, attr
, contents
, rawElement
, child
, children
-- * Parsing contained string data
, Parse
, string
, number
, reader
-- * Selecting Elements
, Selector
, byTag
, any
) where

import           Prelude hiding (any, elem)

import           Control.Applicative (Alternative(..))
import qualified Control.Monad.Fail as M
import qualified Control.Monad.Except as M
import qualified Control.Monad.Reader as M
import qualified GHC.Exts as GHC
import qualified Data.Functor.Identity as M
import qualified Text.XML.Light as XML

-- | The 'XlebT' monad transformer describes a computation used to
-- parse a fragment of XML from a particular element of an XML
-- structure. This may fail with an error, or it may produce a value.
newtype XlebT m a =
  Xleb (M.ReaderT XML.Element (M.ExceptT XlebError m) a)
  deriving (Functor, Applicative, Monad, Alternative)

-- | The 'Xleb' monad describes a computation used to parse a fragment
-- of XML from a particular element of an XML structure. This may fail
-- with an error, or it may produce a value.
type Xleb a = XlebT M.Identity a

-- | The 'XlebError' type describes the various errors that can occur
-- in the course of parsing an XML structure. If you simply want the
-- human-readable string that corresponds to your error, then use the
-- 'errorString' function.
data XlebError
  = XEInElem String XlebError
    -- ^ Describes the element context in which an error occurred
  | XEInAttr String XlebError
    -- ^ Describes the attribute context in which an error occurred
  | XEParseFailure String
    -- ^ Some parser function was unable to produce a value from the
    -- string embedded in an XML element
  | XENoSuchAttribute String
    -- ^ A 'XlebT' computation required an attribute that wasn't
    -- found in the specified element.
  | XEUnexpectedElement String String
    -- ^ A 'XlebT' computation expected one element but found another
  | XENoMatchingElement Selector
    -- ^ A 'XlebT' computation used a selector which did not
    -- successfully describe any child elements
  | XEAmbiguousElement Selector
    -- ^ A 'XlebT' computation used a selector as though it would
    -- unambiguously name a single child, but instead multiple child
    -- elements matched the selector
  | XEBadXML
    -- ^ The "xml" library was unable to parse the document as XML.
  | XOtherError String
    -- ^ Another error occurred which was not described by the above
    -- constructors
    deriving (Eq, Show)

instance Monoid XlebError where
  mappend x _ = x
  mempty = XOtherError "unknown error"

-- | Convert a 'XlebError' value to the corresponding human-readable
-- string.
errorString :: XlebError -> String
errorString = gatherContext ""
  where gatherContext ctx (XEInElem el err) =
          gatherContext (ctx ++ el ++ "/") err
        gatherContext ctx (XEInAttr at err) =
          gatherContext (ctx ++ "[@" ++ at ++ "]") err
        gatherContext ctx err =
          ctx ++ ": " ++ showError err
        showError (XEParseFailure err) = err
        showError XEBadXML =
          "Unable to parse input string as XML"
        showError (XENoSuchAttribute str) =
          "No attribute called '" ++ str ++ "'"
        showError (XEUnexpectedElement e1 e2) =
          "Unexpected element " ++ e1 ++ "; expected " ++ e2
        showError (XENoMatchingElement sel) =
          "No elements were found maching selector " ++ show sel
        showError (XEAmbiguousElement sel) =
          "Multiple elements matched the selector " ++ show sel
        showError (XOtherError str) = str
        showError (XEInElem _ _) = error "[unexpected]"
        showError (XEInAttr _ _) = error "[unexpected]"

instance Monad m => M.MonadFail (XlebT m) where
  fail = Xleb . M.throwError . XOtherError

-- | A value of type @'Parse' t@ is a function that can either produce
-- a value of type @t@ or fail with a string message.
type Parse t = String -> Either String t

-- | A 'Selector' represents some criteria by which child elements are
-- matched.
data Selector
  = SelByName String
  | SelByNS String
  | SelBoth Selector Selector
  | SelAny
    deriving (Eq, Show)

instance Monoid Selector where
  mempty = SelAny
  mappend = SelBoth

instance GHC.IsString Selector where
  fromString = SelByName

toPred :: Selector -> XML.Element -> Bool
toPred SelAny _ = True
toPred (SelByName n) el =
  XML.showQName (XML.elName el) == n
toPred (SelByNS n) el =
  case XML.qPrefix (XML.elName el) of
    Nothing -> False
    Just p  -> p == n
toPred (SelBoth s1 s2) el =
  toPred s1 el && toPred s2 el

-- | Find an attribute on the current focus element and parse it to a
-- value of type @t@. If the parse function fails, then this will fail
-- with 'XEParseFailure'.
attr :: Monad m => String -> Parse t -> XlebT m t
attr name parser = Xleb $ do
  el <- M.ask
  case XML.findAttr (XML.unqual name) el of
    Nothing   -> M.throwError (XENoSuchAttribute name)
    Just a -> case parser a of
      Left err -> M.throwError (XEInAttr name (XEParseFailure err))
      Right x  -> return x

-- | Take the string content of the current element and parse it to a
-- value of type @t@. If the parse function fails, then this will fail
-- with 'XEParseFailure'.
contents :: Monad m => Parse t -> XlebT m t
contents parser = Xleb $ do
  cnt <- XML.strContent `fmap` M.ask
  case parser cnt of
    Left err -> M.throwError (XEParseFailure err)
    Right x  -> return x

-- | Access the raw underlying XML element that we are
-- processing. This is sometimes necessary for working with free-form
-- XML data.
rawElement :: Monad m => XlebT m XML.Element
rawElement = Xleb M.ask

-- | Use a 'Selector' that unambiguously identifies a single child
-- element of the current element and then parse it according to a
-- given 'XlebT' computation focused on that element. If no child
-- matches the provided 'Selector', then this will fail with
-- 'XENoMatchingElement'. If multiple children match the provided
-- 'Selector', then this will fail with 'XEAmbiguousElement'.
child :: Monad m => Selector -> XlebT m t -> XlebT m t
child sel (Xleb mote) = Xleb $ do
  cld <- XML.filterChildren (toPred sel) `fmap` M.ask
  case cld of
    []  -> M.throwError (XENoMatchingElement sel)
    [x] -> M.local (const x) mote
    _   -> M.throwError (XEAmbiguousElement sel)

-- | Use a 'Selector' that identifies some child elements of the
-- current element and parse each according to a given 'XlebT'
-- computation, which will be repeated with focus on each child
-- element, and returning the resulting values as a list. If no child
-- elements match the 'Selector', then this will return an empty list.
children :: Monad m => Selector -> XlebT m t -> XlebT m [t]
children sel (Xleb mote) = Xleb $ do
  cld <- XML.filterChildren (toPred sel) `fmap` M.ask
  sequence [ M.local (const x) mote | x <- cld ]

-- | A 'Parse' function that parses numeric values according to their
-- Haskell 'Read' instance.
number :: (Read n, Num n) => Parse n
number = Right . read

-- | A 'Parse' function that accepts arbitrary string input without
-- failing.
string :: Parse String
string = Right

-- | A 'Parse' function that parses Haskell values according to their
-- 'Read' instance.
reader :: Read a => Parse a
reader = Right . read

-- | Creates a 'Selector' which expects an exact tag name.
byTag :: String -> Selector
byTag = SelByName

-- | Creates a 'Selector' which expects a specific namespace
byNamespace :: String -> Selector
byNamespace = SelByNS

-- | Creates a 'Selector' which matches any possible child element.
any :: Selector
any = SelAny

-- | @'elem' n t@ will ensure that the currently focused element is a
-- tag named @n@ and will then evaluate it using the computation
-- @t@. This will fail with 'XEUnexpectedElement' if the tag is named
-- something else.
elem :: Monad m => String -> XlebT m t -> XlebT m t
elem name (Xleb mote) = Xleb $ do
  el <- M.ask
  case el of
    XML.Element { XML.elName = qname }
      | XML.showQName qname == name -> mote
      | otherwise -> M.throwError
        (XEUnexpectedElement (XML.showQName qname) name)

doXleb :: XML.Element -> XlebT m t -> m (Either XlebError t)
doXleb el (Xleb mote) =
  M.runExceptT (M.runReaderT mote el)

-- | Run a 'Xleb' computation over a string containing XML data,
-- producing either the resulting value or an error. If the XML data
-- contained in the argument string is invalid, then this will fail
-- with 'XEBadXML'.
runXleb :: String -> Xleb t -> Either XlebError t
runXleb raw xleb = case XML.parseXMLDoc raw of
  Nothing -> Left XEBadXML
  Just x  -> M.runIdentity (doXleb x xleb)

-- | Run a 'XlebT' computation over a string containing XML data,
-- producing either the resulting monadic value or an error. If the
-- XML data contained in the argument string is invalid, then this
-- will fail with 'XEBadXML'.
runXlebT :: Monad m => String -> XlebT m t -> m (Either XlebError t)
runXlebT raw xleb = case XML.parseXMLDoc raw of
  Nothing -> return (Left XEBadXML)
  Just x  -> doXleb x xleb

{- $use

The 'Xleb' monad describes both parsing /and/ traversing a given XML
structure: several of the functions to produce 'Xleb' computations
take other 'Xleb' computations, which are run on various sub-parts of
the XML tree. Consequently, instead of decomposing an XML structure
and passing it around to various functions, the 'Xleb' language treats
"the current location in the tree" as an implicit piece of data in the
'Xleb' monad.

You will generally want to identify your root note with the 'elem'
function to ensure that your root note has the tag you
expect. Children of that node can be accessed using the 'child' or
'children' function to either unambiguously find a specific child
element, or to find all child elements that match a given selector and
apply a 'Xleb' computation to each of them.

@
  a <- X.child (X.byTag "a") parseA
  b <- X.children (X.byTag "b") parseB
@

Leaf data tends to come in two forms in XML: attribute values (like
@\<tag attr="value"\>@) or tag content (like
@\<tag\>value\<\/tag\>@). In both cases, the 'Xleb' functions allow
you to parse that content however you'd like by providing an arbitrary
function of type @'String' -> 'Either' 'String' a@. The "xleb" library
provides several built-in functions of this type for common
situations.

@
  c <- X.attr "index" X.number
  d <- X.contents X.string
@

Finally, the `Xleb` monad has `Alternative` instances which allow for
concise expression of optional values or multiple possibilities.

@
  e \<- X.children X.any (parseA \<|\> parseB)
  f \<- optional (X.attr "total" X.number)
@

Consequently, for an XML structure like the following:

@
\<feed\>
  \<title\>Feed Name\<\/title\>
  \<author\>Pierre Menard\<\/author\>
  \<entry title="Entry 01"\>First Post\<\/entry\>
  \<entry title="Entry 02"\>Second Post Post\<\/entry\>
\<\/feed\>
@

We can write a 'Xleb' computation which is capable of parsing this
structure in a handful of lines:

@
import           Control.Applicative (optional)
import qualified Text.XML.Xleb as X

feed :: X.Xleb (String, Maybe String, [(String, String)])
feed = X.elem "feed" $ do
  feedTitle   <- X.child (X.byTag "title") $
                   X.contents X.string
  feedAuthor  <- optional $ X.child (X.byTag "author") $
                              X.contents X.string
  feedEntries <- X.children (X.byTag "entry") entry
  return (feedTitle, feedAuthor, feedEntries)

entry :: X.Xleb (String, String)
entry = (,) \<$\> X.attr "title" X.string \<*\> X.contents X.string
@

-}
