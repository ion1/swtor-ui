{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SWTOR.UIProfile.XML
( placementsToXMLProfile
) where

import Control.Lens
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T

import SWTOR.UIProfile.Internal.Default
import SWTOR.UIProfile.Layout
import SWTOR.UIProfile.Placement
import SWTOR.UIProfile.XMLSerialization

placementsToXMLProfile :: Globals -> [Placement] -> XMLProfile
placementsToXMLProfile globs placs =
  foldl' updateElem defaultXMLProfile
         (globalsToXMLElement globs : map placementToXMLElement placs)

updateElem :: XMLProfile -> (T.Text, XMLElement) -> XMLProfile
updateElem prof (name, XMLElement props)
  | has (_XMLProfile . ix name) prof =
      prof & _XMLProfile . ix name %~ \el -> foldl' updateProp el (M.toList props)
  | otherwise = error $ "Internal error: profile did not have element " ++ show name

updateProp :: XMLElement -> (T.Text, XMLProperty) -> XMLElement
updateProp el (name, prop)
  | el ^? _XMLElement . ix name . to xmlPropertyType == Just (xmlPropertyType prop) =
      el & _XMLElement . ix name .~ prop
  | Just oldProp <- el ^? _XMLElement . ix name =
      error $ "Internal error: property " ++ show name ++ " can not change type: "
           ++ show oldProp ++ " -> " ++ show prop
  | otherwise = error $ "Internal error: property " ++ show name ++ " does not exist"

globalsToXMLElement :: Globals -> (T.Text, XMLElement)
globalsToXMLElement Globals{..} =
  ("Global", XMLElement (M.singleton "GlobalScale" (XMLDouble globScale)))

placementToXMLElement :: Placement -> (T.Text, XMLElement)
placementToXMLElement Placement{..} =
  case elementToXMLElement' placElement of
    (name, props) -> (name, XMLElement (M.fromList (props ++ alignProps)))
  where
    alignProps =
      [ ("anchorAlignment", XMLDouble (alignmentNumber placAlign))
      , (if isWindow then "anchorOffsetX" else "anchorXOffset", XMLDouble (fst placPos))
      , (if isWindow then "anchorOffsetY" else "anchorYOffset", XMLDouble (snd placPos))
      ]
    isWindow | ChatPanel1{}  <- placElement = True
             | ChatPanel2{}  <- placElement = True
             | ChatPanel3{}  <- placElement = True
             | ChatPanel4{}  <- placElement = True
             | ChatPanel5{}  <- placElement = True
             | ChatPanel6{}  <- placElement = True
             | ChatPanel7{}  <- placElement = True
             | ChatPanel8{}  <- placElement = True
             | ChatPanel9{}  <- placElement = True
             | ChatPanel10{} <- placElement = True
             | otherwise = False

elementToXMLElement' :: Element -> (T.Text, [(T.Text, XMLProperty)])
elementToXMLElement' AchievementTracker{..} =
  (,) "AchievementTracker"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlippedVertically", XMLBool elemFlipVertical)
      , ("Height", XMLDouble elemHeight)
      ]
elementToXMLElement' CartelMarket{..} =
  (,) "MTXStorePanel"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' ChatPanel1{..} =
  (,) "ChatPanel_1"
      [ ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel2{..} =
  (,) "ChatPanel_2"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel3{..} =
  (,) "ChatPanel_3"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel4{..} =
  (,) "ChatPanel_4"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel5{..} =
  (,) "ChatPanel_5"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel6{..} =
  (,) "ChatPanel_6"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel7{..} =
  (,) "ChatPanel_7"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel8{..} =
  (,) "ChatPanel_8"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel9{..} =
  (,) "ChatPanel_9"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' ChatPanel10{..} =
  (,) "ChatPanel_10"
      [ ("isPanel", XMLBool True)
      , ("width", XMLDouble (fst elemSize))
      , ("height", XMLDouble (snd elemSize))
      , ("FontSize", XMLDouble elemFontSize)
      ]
elementToXMLElement' CombatState{..} =
  (,) "CombatState"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' Companion{..} =
  (,) "Companion"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' CompanionCastbar{..} =
  (,) "CompanionCastbar"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' ExperienceBars{..} =
  (,) "ExperienceBars"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("ShowXP", XMLBool elemShowXP)
      , ("ShowLegacyXP", XMLBool elemShowLegacyXP)
      ]
elementToXMLElement' FocusTarget{..} =
  (,) "FocusTarget"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      , ("ShowPersonalHighlightBuffs", XMLBool elemShowPersonalHighlightBuffs)
      , ("PersonalHighlightBuffsMaxDuration", XMLDouble elemPersonalHighlightBuffsMaxDuration)
      , ("ShowPersonalHighlightDebuffs", XMLBool elemShowPersonalHighlightDebuffs)
      , ("PersonalHighlightDebuffsMaxDuration", XMLDouble elemPersonalHighlightDebuffsMaxDuration)
      , ("ShowPersonalBuffsFirst", XMLBool elemShowPersonalBuffsFirst)
      , ("BuffsSortType", XMLDouble (effectSortTypeNumber elemBuffsSortType))
      , ("ShowPersonalDebuffsFirst", XMLBool elemShowPersonalDebuffsFirst)
      , ("DebuffsSortType", XMLDouble (effectSortTypeNumber elemDebuffsSortType))
      ]
elementToXMLElement' FocusTargetCastbar{..} =
  (,) "FocusTargetCastbar"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' GameDownload{..} =
  (,) "GameDownload"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' GroupFrame1{..} =
  (,) "PartyFrame1"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      ]
elementToXMLElement' GroupFrame2{..} =
  (,) "PartyFrame2"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      ]
elementToXMLElement' GroupFrame3{..} =
  (,) "PartyFrame3"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      ]
elementToXMLElement' GroupTargetFrame1{..} =
  (,) "PartyTargetFrame1"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      ]
elementToXMLElement' GroupTargetFrame2{..} =
  (,) "PartyTargetFrame2"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      ]
elementToXMLElement' GroupTargetFrame3{..} =
  (,) "PartyTargetFrame3"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      ]
elementToXMLElement' Holocom{..} =
  (,) "Holocom"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' LargeTooltip{..} =
  (,) "LargeTooltip"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("GrowUp", XMLBool elemGrowUp)
      , ("UseDockedStyle", XMLBool elemAttachToMiniMap)
      ]
elementToXMLElement' MenuBar{..} =
  (,) "MenuBar"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("NumPerRow", XMLDouble (fromInteger elemNumPerRow))
      ]
elementToXMLElement' MiniMap{..} =
  (,) "MiniMap"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipVertically", XMLBool elemFlipVertical)
      ]
elementToXMLElement' MissionTracker{..} =
  (,) "MissionTracker"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlippedVertically", XMLBool elemFlipVertical)
      , ("Height", XMLDouble elemHeight)
      ]
elementToXMLElement' OperationFrames{..} =
  (,) "RaidFrames"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("NumPerRow", XMLDouble (fromIntegral elemNumPerRow))
      , ("GroupsVisible", XMLDouble (fromIntegral elemNumVisible))
      , ("HealthWidth", XMLDouble (fst elemHealthSize))
      , ("HealthHeight", XMLDouble (snd elemHealthSize))
      , ("ShowHealth", XMLBool elemShowHealth)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      , ("PartySpacing", XMLDouble elemPartySpacing)
      , ("ShowOnlyCleansableDebuffs", XMLBool elemShowOnlyCleansableDebuffs)
      ]
elementToXMLElement' PhaseIndicator{..} =
  (,) "PhaseIndicator"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' PlayerBuffTray{..} =
  (,) "PlayerPositiveEffectTray"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("TrayIconsScale", XMLDouble elemIconScale)
      , ("TrayMaxColumns", XMLDouble (fromIntegral elemNumPerRow))
      , ("TraySortType", XMLDouble (effectSortTypeNumber elemBuffsSortType))
      , ("TrayShowPersonalHighlight", XMLBool elemShowPersonalHighlightBuffs)
      , ("TrayPersonalHighlightMaxDuration", XMLDouble elemPersonalHighlightBuffsMaxDuration)
      , ("TrayShowPersonalEffectsFirst", XMLBool elemShowPersonalBuffsFirst)
      ]
elementToXMLElement' PlayerCastbar{..} =
  (,) "PlayerCastbar"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' PlayerDebuffTray{..} =
  (,) "PlayerNegativeEffectTray"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("TrayIconsScale", XMLDouble elemIconScale)
      , ("TrayMaxColumns", XMLDouble (fromIntegral elemNumPerRow))
      , ("TraySortType", XMLDouble (effectSortTypeNumber elemDebuffsSortType))
      , ("TrayShowPersonalHighlight", XMLBool elemShowPersonalHighlightDebuffs)
      , ("TrayPersonalHighlightMaxDuration", XMLDouble elemPersonalHighlightDebuffsMaxDuration)
      , ("TrayShowPersonalEffectsFirst", XMLBool elemShowPersonalDebuffsFirst)
      ]
elementToXMLElement' PlayerFrame{..} =
  (,) "PlayerPortrait"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowRole", XMLBool elemShowRole)
      ]
elementToXMLElement' QuickBar1{..} =
  (,) "QuickBar1"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", XMLDouble (fromInteger elemNumVisible))
      , ("NumPerRow", XMLDouble (fromInteger elemNumPerRow))
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar2{..} =
  (,) "QuickBar2"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", XMLDouble (fromInteger elemNumVisible))
      , ("NumPerRow", XMLDouble (fromInteger elemNumPerRow))
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar3{..} =
  (,) "QuickBar3"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", XMLDouble (fromInteger elemNumVisible))
      , ("NumPerRow", XMLDouble (fromInteger elemNumPerRow))
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar4{..} =
  (,) "QuickBar4"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", XMLDouble (fromInteger elemNumVisible))
      , ("NumPerRow", XMLDouble (fromInteger elemNumPerRow))
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar5{..} =
  (,) "QuickBar5"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", XMLDouble (fromInteger elemNumVisible))
      , ("NumPerRow", XMLDouble (fromInteger elemNumPerRow))
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar6{..} =
  (,) "QuickBar6"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", XMLDouble (fromInteger elemNumVisible))
      , ("NumPerRow", XMLDouble (fromInteger elemNumPerRow))
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' SocialCenter{..} =
  (,) "SocialCenter"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' SocialNotifications{..} =
  (,) "SocialNotifications"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' SystemMessages{..} =
  (,) "SystemMessages"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' TargetCastbar{..} =
  (,) "TargetCastbar"
      [ ("scale", XMLDouble elemScale)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' TargetFrame{..} =
  (,) "TargetPortrait"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowNoCharacter", XMLBool elemDisplayWithNoTarget)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      , ("ShowPersonalHighlightBuffs", XMLBool elemShowPersonalHighlightBuffs)
      , ("PersonalHighlightBuffsMaxDuration", XMLDouble elemPersonalHighlightBuffsMaxDuration)
      , ("ShowPersonalHighlightDebuffs", XMLBool elemShowPersonalHighlightDebuffs)
      , ("PersonalHighlightDebuffsMaxDuration", XMLDouble elemPersonalHighlightDebuffsMaxDuration)
      , ("ShowPersonalBuffsFirst", XMLBool elemShowPersonalBuffsFirst)
      , ("BuffsSortType", XMLDouble (effectSortTypeNumber elemBuffsSortType))
      , ("ShowPersonalDebuffsFirst", XMLBool elemShowPersonalDebuffsFirst)
      , ("DebuffsSortType", XMLDouble (effectSortTypeNumber elemDebuffsSortType))
      ]
elementToXMLElement' TargetOfTarget{..} =
  (,) "TargetOfTarget"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("BuffScale", XMLDouble elemBuffScale)
      , ("DebuffScale", XMLDouble elemDebuffScale)
      , ("ShowRole", XMLBool elemShowRole)
      ]
elementToXMLElement' TargetOfTargetCastbar{..} =
  (,) "TargetOfTargetCastbar"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' TemporaryAbilityBar{..} =
  (,) "TemporaryAbilityBar"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' Tutorials{..} =
  (,) "Tutorials"
      [ ("scale", XMLDouble elemScale)
      , ("enabled", XMLBool True)
      ]

alignmentNumber :: Num n => Alignment -> n
alignmentNumber al =
  case al of
    TL -> 1
    BL -> 2
    L  -> 3
    TR -> 4
    BR -> 5
    R  -> 6
    T  -> 7
    B  -> 8
    C  -> 9

effectSortTypeNumber :: Num n => EffectSortType -> n
effectSortTypeNumber est =
  case est of
    ApplyTime       -> 0
    TotalDuration   -> 1
    ByTimeRemaining -> 2
