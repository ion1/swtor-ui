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

xmlReal :: Real n => n -> XMLProperty
xmlReal n = XMLReal (realToFrac n)

globalsToXMLElement :: Globals -> (T.Text, XMLElement)
globalsToXMLElement Globals{..} =
  ("Global", XMLElement (M.singleton "GlobalScale" (xmlReal globScale)))

placementToXMLElement :: Placement -> (T.Text, XMLElement)
placementToXMLElement Placement{..} =
  case elementToXMLElement' placElement of
    (name, props) -> (name, XMLElement (M.fromList (props ++ alignProps)))
  where
    alignProps =
      [ ("anchorAlignment", xmlReal (alignmentNumber placAlign))
      , (if isWindow then "anchorOffsetX" else "anchorXOffset", xmlReal (fst placPos))
      , (if isWindow then "anchorOffsetY" else "anchorYOffset", xmlReal (snd placPos))
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
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlippedVertically", XMLBool elemFlipVertical)
      , ("Height", xmlReal elemHeight)
      ]
elementToXMLElement' CartelMarket{..} =
  (,) "MTXStorePanel"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' ChatPanel1{..} =
  (,) "ChatPanel_1"
      [ ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel2{..} =
  (,) "ChatPanel_2"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel3{..} =
  (,) "ChatPanel_3"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel4{..} =
  (,) "ChatPanel_4"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel5{..} =
  (,) "ChatPanel_5"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel6{..} =
  (,) "ChatPanel_6"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel7{..} =
  (,) "ChatPanel_7"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel8{..} =
  (,) "ChatPanel_8"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel9{..} =
  (,) "ChatPanel_9"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' ChatPanel10{..} =
  (,) "ChatPanel_10"
      [ ("isPanel", XMLBool True)
      , ("width", xmlReal (fst elemSize))
      , ("height", xmlReal (snd elemSize))
      , ("FontSize", xmlReal elemFontSize)
      ]
elementToXMLElement' CombatState{..} =
  (,) "CombatState"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' Companion{..} =
  (,) "Companion"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' CompanionCastbar{..} =
  (,) "CompanionCastbar"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' ExperienceBars{..} =
  (,) "ExperienceBars"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("ShowXP", XMLBool elemShowXP)
      , ("ShowLegacyXP", XMLBool elemShowLegacyXP)
      ]
elementToXMLElement' FocusTarget{..} =
  (,) "FocusTarget"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      , ("ShowPersonalHighlightBuffs", XMLBool elemShowPersonalHighlightBuffs)
      , ("PersonalHighlightBuffsMaxDuration", xmlReal elemPersonalHighlightBuffsMaxDuration)
      , ("ShowPersonalHighlightDebuffs", XMLBool elemShowPersonalHighlightDebuffs)
      , ("PersonalHighlightDebuffsMaxDuration", xmlReal elemPersonalHighlightDebuffsMaxDuration)
      , ("ShowPersonalBuffsFirst", XMLBool elemShowPersonalBuffsFirst)
      , ("BuffsSortType", xmlReal (effectSortTypeNumber elemBuffsSortType))
      , ("ShowPersonalDebuffsFirst", XMLBool elemShowPersonalDebuffsFirst)
      , ("DebuffsSortType", xmlReal (effectSortTypeNumber elemDebuffsSortType))
      ]
elementToXMLElement' FocusTargetCastbar{..} =
  (,) "FocusTargetCastbar"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' GameDownload{..} =
  (,) "GameDownload"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' GroupFrame1{..} =
  (,) "PartyFrame1"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      ]
elementToXMLElement' GroupFrame2{..} =
  (,) "PartyFrame2"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      ]
elementToXMLElement' GroupFrame3{..} =
  (,) "PartyFrame3"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      ]
elementToXMLElement' GroupTargetFrame1{..} =
  (,) "PartyTargetFrame1"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      ]
elementToXMLElement' GroupTargetFrame2{..} =
  (,) "PartyTargetFrame2"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      ]
elementToXMLElement' GroupTargetFrame3{..} =
  (,) "PartyTargetFrame3"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      ]
elementToXMLElement' Holocom{..} =
  (,) "Holocom"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' LargeTooltip{..} =
  (,) "LargeTooltip"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("GrowUp", XMLBool elemGrowUp)
      , ("UseDockedStyle", XMLBool elemAttachToMiniMap)
      ]
elementToXMLElement' MenuBar{..} =
  (,) "MenuBar"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("NumPerRow", xmlReal elemNumPerRow)
      ]
elementToXMLElement' MiniMap{..} =
  (,) "MiniMap"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipVertically", XMLBool elemFlipVertical)
      ]
elementToXMLElement' MissionTracker{..} =
  (,) "MissionTracker"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlippedVertically", XMLBool elemFlipVertical)
      , ("Height", xmlReal elemHeight)
      ]
elementToXMLElement' OperationFrames{..} =
  (,) "RaidFrames"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("NumPerRow", xmlReal elemNumPerRow)
      , ("GroupsVisible", xmlReal elemNumVisible)
      , ("HealthWidth", xmlReal (fst elemHealthSize))
      , ("HealthHeight", xmlReal (snd elemHealthSize))
      , ("ShowHealth", XMLBool elemShowHealth)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      , ("PartySpacing", xmlReal elemPartySpacing)
      , ("ShowOnlyCleansableDebuffs", XMLBool elemShowOnlyCleansableDebuffs)
      ]
elementToXMLElement' PhaseIndicator{..} =
  (,) "PhaseIndicator"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' PlayerBuffTray{..} =
  (,) "PlayerPositiveEffectTray"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("TrayIconsScale", xmlReal elemIconScale)
      , ("TrayMaxColumns", xmlReal elemNumPerRow)
      , ("TrayExpandsVertical", XMLBool (expandVerticalBool elemExpandVertical))
      , ("TrayExpandsHorizontal", XMLBool (expandHorizontalBool elemExpandHorizontal))
      , ("TraySortType", xmlReal (effectSortTypeNumber elemBuffsSortType))
      , ("TrayShowPersonalHighlight", XMLBool elemShowPersonalHighlightBuffs)
      , ("TrayPersonalHighlightMaxDuration", xmlReal elemPersonalHighlightBuffsMaxDuration)
      , ("TrayShowPersonalEffectsFirst", XMLBool elemShowPersonalBuffsFirst)
      ]
elementToXMLElement' PlayerCastbar{..} =
  (,) "PlayerCastbar"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' PlayerDebuffTray{..} =
  (,) "PlayerNegativeEffectTray"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("TrayIconsScale", xmlReal elemIconScale)
      , ("TrayMaxColumns", xmlReal elemNumPerRow)
      , ("TrayExpandsVertical", XMLBool (expandVerticalBool elemExpandVertical))
      , ("TrayExpandsHorizontal", XMLBool (expandHorizontalBool elemExpandHorizontal))
      , ("TraySortType", xmlReal (effectSortTypeNumber elemDebuffsSortType))
      , ("TrayShowPersonalHighlight", XMLBool elemShowPersonalHighlightDebuffs)
      , ("TrayPersonalHighlightMaxDuration", xmlReal elemPersonalHighlightDebuffsMaxDuration)
      , ("TrayShowPersonalEffectsFirst", XMLBool elemShowPersonalDebuffsFirst)
      ]
elementToXMLElement' PlayerFrame{..} =
  (,) "PlayerPortrait"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowRole", XMLBool elemShowRole)
      ]
elementToXMLElement' QuickBar1{..} =
  (,) "QuickBar1"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", xmlReal elemNumVisible)
      , ("NumPerRow", xmlReal elemNumPerRow)
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar2{..} =
  (,) "QuickBar2"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", xmlReal elemNumVisible)
      , ("NumPerRow", xmlReal elemNumPerRow)
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar3{..} =
  (,) "QuickBar3"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", xmlReal elemNumVisible)
      , ("NumPerRow", xmlReal elemNumPerRow)
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar4{..} =
  (,) "QuickBar4"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", xmlReal elemNumVisible)
      , ("NumPerRow", xmlReal elemNumPerRow)
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar5{..} =
  (,) "QuickBar5"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", xmlReal elemNumVisible)
      , ("NumPerRow", xmlReal elemNumPerRow)
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' QuickBar6{..} =
  (,) "QuickBar6"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("NumVisible", xmlReal elemNumVisible)
      , ("NumPerRow", xmlReal elemNumPerRow)
      , ("BGVisible", XMLBool elemBGVisible)
      ]
elementToXMLElement' SocialCenter{..} =
  (,) "SocialCenter"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' SocialNotifications{..} =
  (,) "SocialNotifications"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' SystemMessages{..} =
  (,) "SystemMessages"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' TargetCastbar{..} =
  (,) "TargetCastbar"
      [ ("scale", xmlReal elemScale)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' TargetFrame{..} =
  (,) "TargetPortrait"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("ShowNoCharacter", XMLBool elemDisplayWithNoTarget)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("ShowRole", XMLBool elemShowRole)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      , ("ShowPersonalHighlightBuffs", XMLBool elemShowPersonalHighlightBuffs)
      , ("PersonalHighlightBuffsMaxDuration", xmlReal elemPersonalHighlightBuffsMaxDuration)
      , ("ShowPersonalHighlightDebuffs", XMLBool elemShowPersonalHighlightDebuffs)
      , ("PersonalHighlightDebuffsMaxDuration", xmlReal elemPersonalHighlightDebuffsMaxDuration)
      , ("ShowPersonalBuffsFirst", XMLBool elemShowPersonalBuffsFirst)
      , ("BuffsSortType", xmlReal (effectSortTypeNumber elemBuffsSortType))
      , ("ShowPersonalDebuffsFirst", XMLBool elemShowPersonalDebuffsFirst)
      , ("DebuffsSortType", xmlReal (effectSortTypeNumber elemDebuffsSortType))
      ]
elementToXMLElement' TargetOfTarget{..} =
  (,) "TargetOfTarget"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      , ("ShowInfoText", XMLBool elemShowInfoText)
      , ("EffectsOnBottom", XMLBool elemEffectsOnBottom)
      , ("BuffScale", xmlReal elemBuffScale)
      , ("DebuffScale", xmlReal elemDebuffScale)
      , ("ShowRole", XMLBool elemShowRole)
      ]
elementToXMLElement' TargetOfTargetCastbar{..} =
  (,) "TargetOfTargetCastbar"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      , ("FlipHorizontal", XMLBool elemFlipHorizontal)
      ]
elementToXMLElement' TemporaryAbilityBar{..} =
  (,) "TemporaryAbilityBar"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]
elementToXMLElement' Tutorials{..} =
  (,) "Tutorials"
      [ ("scale", xmlReal elemScale)
      , ("enabled", XMLBool True)
      ]

alignmentNumber :: Alignment -> Integer
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

effectSortTypeNumber :: EffectSortType -> Integer
effectSortTypeNumber est =
  case est of
    ApplyTime       -> 0
    TotalDuration   -> 1
    ByTimeRemaining -> 2

expandVerticalBool :: ExpandVertical -> Bool
expandVerticalBool ExpandDown = False
expandVerticalBool ExpandUp   = True

expandHorizontalBool :: ExpandHorizontal -> Bool
expandHorizontalBool ExpandLeft  = False
expandHorizontalBool ExpandRight = True
