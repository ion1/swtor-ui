module SWTOR.UIProfile
( module SWTOR.UIProfile.Layout
, writeLayout, getProfilePath
) where

import Control.DeepSeq
import Control.Exception
import System.Directory

import SWTOR.UIProfile.Layout
import SWTOR.UIProfile.Placement
import SWTOR.UIProfile.XML
import SWTOR.UIProfile.XMLSerialization

writeLayout :: FilePath -> Layout -> IO ()
writeLayout path ly = do
  xmlProf <- evaluate $!! uncurry placementsToXMLProfile (place ly)
  writeProfile path xmlProf

-- | Determine the full path to a named profile.
-- >>> profilePath "Default"
-- "C:\\Users\\ion\\AppData\\Roaming\\../Local/SWTOR/swtor/settings/GUIProfiles/Default.xml"
getProfilePath :: FilePath -> IO FilePath
getProfilePath name = do
  profileDir <- getAppUserDataDirectory "../Local/SWTOR/swtor/settings/GUIProfiles"
  pure (profileDir ++ "/" ++ name ++ ".xml")
