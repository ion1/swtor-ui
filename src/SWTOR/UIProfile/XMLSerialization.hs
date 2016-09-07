{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module SWTOR.UIProfile.XMLSerialization
( XMLProfile (..), _XMLProfile
, XMLElement (..), _XMLElement
, XMLProperty (..), _XMLBool, _XMLReal
, readProfile, writeProfile
, xmlPropertyType
) where

import Control.DeepSeq
import Control.Lens.TH
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.XML.Types (Event)
import GHC.Generics
import qualified Text.XML as X
import qualified Text.XML.Stream.Parse as X

data XMLProfile = XMLProfile (M.Map T.Text XMLElement)
  deriving (Eq, Ord, Read, Show, Generic, NFData)

data XMLElement = XMLElement (M.Map T.Text XMLProperty)
  deriving (Eq, Ord, Read, Show, Generic, NFData)

data XMLProperty = XMLBool Bool
                 | XMLReal Double
  deriving (Eq, Ord, Read, Show, Generic, NFData)

makePrisms ''XMLProfile
makePrisms ''XMLElement
makePrisms ''XMLProperty

readProfile :: FilePath -> IO XMLProfile
readProfile path =
  runResourceT $
    X.parseFile X.def path $$
      X.force ("Failed to parse profile: " ++ show path) parseProfile

writeProfile :: FilePath -> XMLProfile -> IO ()
writeProfile path prof =
  X.writeFile X.def{X.rsPretty = True} path (renderProfile prof)

parseProfile :: MonadThrow m => ConduitM Event o m (Maybe XMLProfile)
parseProfile = X.tagName "GUIProfile" (X.requireAttr "Version") $ \ver ->
  case ver of
    "1" -> XMLProfile . M.fromList <$> X.many parseElement
    _   -> throwM $ X.XmlException ("Unsupported GUIProfile Version: " ++ show ver) Nothing

parseElement :: MonadThrow m => ConduitM Event o m (Maybe (T.Text, XMLElement))
parseElement = X.tag localName pure $ \name ->
  (,) name . XMLElement . M.fromList <$> X.many parseProperty

parseProperty :: MonadThrow m => ConduitM Event o m (Maybe (T.Text, XMLProperty))
parseProperty = X.tag localName validateAttrs $ \(name, typ, value) ->
  (,) name <$> go name typ value
  where
    validateAttrs name@"FlipHorizontal" =
      -- Some instances of FlipHorizontal are empty.
      (,,) name <$> (fromMaybe "2" <$> X.attr "Type")
                <*> (fromMaybe "0" <$> X.attr "Value")
    validateAttrs name =
      (,,) name <$> X.requireAttr "Type"
                <*> X.requireAttr "Value"

    go name "2" value
      | value == "0" = pure (XMLBool False)
      | value == "1" = pure (XMLBool True)
      | otherwise = throwM $ X.XmlException ("Invalid boolean for " ++ show name ++ ": " ++ show value) Nothing
    go name "3" value
      | [(n, "")] <- (reads . T.unpack) value = pure (XMLReal n)
      | otherwise = throwM $ X.XmlException ("Invalid number for " ++ show name ++ ": " ++ show value) Nothing
    go name typ value =
      throwM $ X.XmlException ("Unrecognized Type for " ++ show name ++ ": " ++ show typ ++ "(Value: " ++ show value ++ ")") Nothing

localName :: X.Name -> Maybe T.Text
localName X.Name{..} =
  nameLocalName <$ guard (isNothing nameNamespace && isNothing namePrefix)

renderProfile :: XMLProfile -> X.Document
renderProfile (XMLProfile elems) =
  X.Document (X.Prologue [] Nothing []) root []
  where
    root = X.Element "GUIProfile" (M.singleton "Version" "1") elemNodes
    elemNodes = foldMap (uncurry renderElement) (M.toList elems)

renderElement :: T.Text -> XMLElement -> [X.Node]
renderElement name (XMLElement props) =
  [ X.NodeElement $ X.Element (X.Name name Nothing Nothing) mempty propNodes ]
  where
    propNodes = foldMap (uncurry renderProperty) (M.toList props)

renderProperty :: T.Text -> XMLProperty -> [X.Node]
renderProperty name prop =
  [ X.NodeElement $ X.Element (X.Name name Nothing Nothing) attrs [] ]
  where
    attrs = M.fromList [("Type", xmlPropertyType prop), ("Value", valueAttr)]
    valueAttr =
      case prop of
        XMLBool b -> if b then "1" else "0"
        XMLReal n -> (T.pack . show) n

xmlPropertyType :: XMLProperty -> T.Text
xmlPropertyType XMLBool{} = "2"
xmlPropertyType XMLReal{} = "3"
