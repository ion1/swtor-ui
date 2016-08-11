-- | Load the Default profile in SWTOR and save it with the name "Default".
-- Then run this to generate Default.hs.

module Main (main) where

import SWTOR.UIProfile
import SWTOR.UIProfile.XMLSerialization

main :: IO ()
main = do
  profilePath <- getProfilePath "Default"
  let hsPath = "src/SWTOR/UIProfile/Internal/Default.hs.new"
  putStrLn ("Reading " ++ show profilePath)
  prof <- readProfile profilePath
  putStrLn ("Writing " ++ show hsPath)
  writeFile hsPath . unlines $
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "-- | This module has been generated automatically by swtor-ui-generate-default."
    , "module SWTOR.UIProfile.Internal.Default (defaultXMLProfile) where"
    , "import Data.Map"
    , "import SWTOR.UIProfile.XMLSerialization"
    , "defaultXMLProfile :: XMLProfile"
    , "defaultXMLProfile = " ++ show prof
    ]
