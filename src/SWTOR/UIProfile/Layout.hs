{-# LANGUAGE RecordWildCards #-}

module SWTOR.UIProfile.Layout
( Layout (..)
, Globals (..), globals
, LayoutPrim (..), (<->)
, layoutPrimParentAlignment
, Alignment (..), EffectSortType (..)
, ExpandVertical (..), ExpandHorizontal (..)
, Element (..)
, achievementTracker, cartelMarket, chatPanel1, chatPanel2, chatPanel3
, chatPanel4, chatPanel5, chatPanel6, chatPanel7, chatPanel8, chatPanel9
, chatPanel10, combatState, companion, companionCastBar, experienceBars
, focusTarget, focusTargetCastbar, gameDownload, groupFrame1, groupFrame2
, groupFrame3, groupTargetFrame1, groupTargetFrame2, groupTargetFrame3
, holocom, largeTooltip, menuBar, miniMap, missionTracker, operationFrames
, phaseIndicator, playerBuffTray, playerCastbar, playerDebuffTray, playerFrame
, quickBar1, quickBar2, quickBar3, quickBar4, quickBar5, quickBar6
, socialCenter, socialNotifications, systemMessages, targetCastbar, targetFrame
, targetOfTarget, targetOfTargetCastbar, temporaryAbilityBar, tutorials
, elementSize
) where

import Data.Foldable

data Layout = Screen { scrGlobals :: Globals
                     , scrPrims :: [LayoutPrim]
                     }
  deriving (Eq, Ord, Show, Read)

data Globals = Globals { globScale :: Double }
  deriving (Eq, Ord, Show, Read)

data LayoutPrim = Anchor { lyParentAlign :: Alignment
                         , lyThisAlign :: Alignment
                         , lyElement :: Element
                         , lyChildren :: [LayoutPrim]
                         }
                | Box { lyInside :: [LayoutPrim]
                      , lyChildren :: [LayoutPrim]
                      }
  deriving (Eq, Ord, Show, Read)

data Alignment = TL {- ^ Top left  -} | BL {- ^ Bottom left  -} | L {- ^ Left  -}
               | TR {- ^ Top right -} | BR {- ^ Bottom right -} | R {- ^ Right -}
               | T {- ^ Top -} | B {- ^ Bottom -} | C {- ^ Center -}
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data EffectSortType = ApplyTime | TotalDuration | ByTimeRemaining
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data ExpandVertical = ExpandDown | ExpandUp
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data ExpandHorizontal = ExpandLeft | ExpandRight
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Element = AchievementTracker { elemScale :: Double  -- ^ Default: 1
                                  , elemFlipVertical :: Bool  -- ^ Default: True
                                  , elemHeight :: Double  -- ^ Default: 295
                                  , elemOffset :: (Double, Double)
                                  }
             | CartelMarket { elemScale :: Double  -- ^ Default: 1
                            , elemOffset :: (Double, Double)
                            }
             | ChatPanel1  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel2  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel3  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel4  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel5  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel6  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel7  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel8  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel9  { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | ChatPanel10 { elemSize :: (Double, Double)  -- ^ Default: (400,200), minimum: (400,185), maximum: (800,500)
                           , elemFontSize :: Double        -- ^ Default: 14
                           , elemOffset :: (Double, Double)
                           }
             | CombatState { elemScale :: Double  -- ^ Default: 1
                           , elemOffset :: (Double, Double)
                           }
             | Companion { elemScale :: Double  -- ^ Default: 1
                         , elemOffset :: (Double, Double)
                         }
             | CompanionCastbar { elemScale :: Double  -- ^ Default: 1
                                , elemFlipHorizontal :: Bool    -- ^ Default: False
                                , elemOffset :: (Double, Double)
                                }
             | ExperienceBars { elemScale        :: Double  -- ^ Default: 1
                              , elemShowXP       :: Bool    -- ^ Default: True
                              , elemShowLegacyXP :: Bool    -- ^ Default: True
                              , elemOffset :: (Double, Double)
                              }
             | FocusTarget { elemScale :: Double  -- ^ Default: 0.8
                           , elemFlipHorizontal :: Bool  -- ^ Default: False
                           , elemShowInfoText :: Bool  -- ^ Default: False
                           , elemBuffScale :: Double  -- ^ Default: 0.6
                           , elemDebuffScale :: Double  -- ^ Default: 0.6
                           , elemShowPersonalHighlightBuffs :: Bool  -- ^ Default: False
                           , elemPersonalHighlightBuffsMaxDuration :: Double  -- ^ Default: 300
                           , elemShowPersonalHighlightDebuffs :: Bool  -- ^ Default: False
                           , elemPersonalHighlightDebuffsMaxDuration :: Double -- ^ Default: 300
                           , elemShowPersonalBuffsFirst :: Bool  -- ^ Default: False
                           , elemBuffsSortType :: EffectSortType  -- ^ Default: ApplyTime
                           , elemShowPersonalDebuffsFirst :: Bool  -- ^ Default: False
                           , elemDebuffsSortType :: EffectSortType  -- ^ Default: ApplyTime
                           , elemOffset :: (Double, Double)
                           }
             | FocusTargetCastbar { elemScale :: Double  -- ^ Default: 0.9
                                  , elemFlipHorizontal :: Bool  -- ^ Default: False
                                  , elemOffset :: (Double, Double)
                                  }
             | GameDownload { elemScale :: Double  -- ^ Default: 1
                            , elemOffset :: (Double, Double)
                            }
             | GroupFrame1 { elemScale :: Double  -- ^ Default: 1
                           , elemFlipHorizontal :: Bool  -- ^ Default: False
                           , elemShowInfoText :: Bool  -- ^ Default: False
                           , elemShowRole :: Bool  -- ^ Default: True
                           , elemBuffScale :: Double  -- ^ Default: 0.35
                           , elemDebuffScale :: Double  -- ^ Default: 0.35
                           , elemOffset :: (Double, Double)
                           }
             | GroupFrame2 { elemScale :: Double  -- ^ Default: 1
                           , elemFlipHorizontal :: Bool  -- ^ Default: False
                           , elemShowInfoText :: Bool  -- ^ Default: False
                           , elemShowRole :: Bool  -- ^ Default: True
                           , elemBuffScale :: Double  -- ^ Default: 0.35
                           , elemDebuffScale :: Double  -- ^ Default: 0.35
                           , elemOffset :: (Double, Double)
                           }
             | GroupFrame3 { elemScale :: Double  -- ^ Default: 1
                           , elemFlipHorizontal :: Bool  -- ^ Default: False
                           , elemShowInfoText :: Bool  -- ^ Default: False
                           , elemShowRole :: Bool  -- ^ Default: True
                           , elemBuffScale :: Double  -- ^ Default: 0.35
                           , elemDebuffScale :: Double  -- ^ Default: 0.35
                           , elemOffset :: (Double, Double)
                           }
             | GroupTargetFrame1 { elemScale :: Double  -- ^ Default: 0.75
                                 , elemFlipHorizontal :: Bool  -- ^ Default: True
                                 , elemShowInfoText :: Bool  -- ^ Default: False
                                 , elemEffectsOnBottom :: Bool  -- ^ Default: False
                                 , elemShowRole :: Bool  -- ^ Default: True
                                 , elemBuffScale :: Double  -- ^ Default: 0.25
                                 , elemDebuffScale :: Double  -- ^ Default: 0.25
                                 , elemOffset :: (Double, Double)
                                 }
             | GroupTargetFrame2 { elemScale :: Double  -- ^ Default: 0.75
                                 , elemFlipHorizontal :: Bool  -- ^ Default: True
                                 , elemShowInfoText :: Bool  -- ^ Default: False
                                 , elemEffectsOnBottom :: Bool  -- ^ Default: False
                                 , elemShowRole :: Bool  -- ^ Default: True
                                 , elemBuffScale :: Double  -- ^ Default: 0.25
                                 , elemDebuffScale :: Double  -- ^ Default: 0.25
                                 , elemOffset :: (Double, Double)
                                 }
             | GroupTargetFrame3 { elemScale :: Double  -- ^ Default: 0.75
                                 , elemFlipHorizontal :: Bool  -- ^ Default: True
                                 , elemShowInfoText :: Bool  -- ^ Default: False
                                 , elemEffectsOnBottom :: Bool  -- ^ Default: False
                                 , elemShowRole :: Bool  -- ^ Default: True
                                 , elemBuffScale :: Double  -- ^ Default: 0.25
                                 , elemDebuffScale :: Double  -- ^ Default: 0.25
                                 , elemOffset :: (Double, Double)
                                 }
             | Holocom { elemScale :: Double  -- ^ Default: 1
                       , elemOffset :: (Double, Double)
                       }
             | LargeTooltip { elemScale :: Double  -- ^ Default: 0.9 from mini-map's scale
                            , elemAttachToMiniMap :: Bool  -- ^ Default: True
                            , elemGrowUp :: Bool  -- ^ Default: True
                            , elemOffset :: (Double, Double)
                            }
             | MenuBar { elemScale :: Double  -- ^ Default: 1
                       , elemNumPerRow :: Integer -- ^ Default: 13
                       , elemOffset :: (Double, Double)
                       }
             | MiniMap { elemScale :: Double  -- ^ Default: 0.9
                       , elemFlipVertical :: Bool  -- ^ Default: False
                       , elemOffset :: (Double, Double)
                       }
             | MissionTracker { elemScale :: Double  -- ^ Default: 1
                              , elemFlipVertical :: Bool  -- ^ Default: False
                              , elemHeight :: Double  -- ^ Default: 500
                              , elemOffset :: (Double, Double)
                              }
             | OperationFrames { elemScale :: Double  -- ^ Default: 1
                               , elemNumPerRow :: Integer  -- ^ Default: 3
                               , elemNumVisible :: Integer  -- ^ Default: 6
                               , elemHealthSize :: (Double, Double)  -- ^ Default: (102,4)
                               , elemShowHealth :: Bool  -- ^ Default: False
                               , elemBuffScale :: Double  -- ^ Default: 0.22
                               , elemDebuffScale :: Double  -- ^ Default: 0.22
                               , elemPartySpacing :: Double  -- ^ Default: 0
                               , elemShowOnlyCleansableDebuffs :: Bool -- ^ Default: False
                               , elemOffset :: (Double, Double)
                               }
             | PhaseIndicator { elemScale :: Double  -- ^ Default: 1
                              , elemOffset :: (Double, Double)
                              }
             | PlayerBuffTray { elemScale :: Double  -- ^ Default: 1
                              , elemIconScale :: Double  -- ^ Default: 0.6
                              , elemNumPerRow :: Integer  -- ^ Default: 5
                              , elemExpandVertical :: ExpandVertical  -- ^ Default: ExpandUp
                              , elemExpandHorizontal :: ExpandHorizontal  -- ^ Default: ExpandRight
                              , elemBuffsSortType :: EffectSortType  -- ^ Default: ApplyTime
                              , elemShowPersonalHighlightBuffs :: Bool  -- ^ Default: False
                              , elemPersonalHighlightBuffsMaxDuration :: Double  -- ^ Default: 300
                              , elemShowPersonalBuffsFirst :: Bool  -- ^ Default: False
                              , elemOffset :: (Double, Double)
                              }
             | PlayerCastbar { elemScale :: Double  -- ^ Default: 1
                             , elemFlipHorizontal :: Bool  -- ^ Default: True
                             , elemOffset :: (Double, Double)
                             }
             | PlayerDebuffTray { elemScale :: Double  -- ^ Default: 1
                                , elemIconScale :: Double  -- ^ Default: 0.6
                                , elemNumPerRow :: Integer  -- ^ Default: 5
                                , elemExpandVertical :: ExpandVertical  -- ^ Default: ExpandUp
                                , elemExpandHorizontal :: ExpandHorizontal  -- ^ Default: ExpandLeft
                                , elemDebuffsSortType :: EffectSortType  -- ^ Default: ApplyTime
                                , elemShowPersonalHighlightDebuffs :: Bool  -- ^ Default: False
                                , elemPersonalHighlightDebuffsMaxDuration :: Double  -- ^ Default: 300
                                , elemShowPersonalDebuffsFirst :: Bool  -- ^ Default: False
                                , elemOffset :: (Double, Double)
                                }
             | PlayerFrame { elemScale :: Double  -- ^ Default: 1
                           , elemFlipHorizontal :: Bool  -- ^ Default: False
                           , elemShowInfoText :: Bool  -- ^ Default: False
                           , elemEffectsOnBottom :: Bool  -- ^ Default: False
                           , elemShowRole :: Bool  -- ^ Default: True
                           , elemOffset :: (Double, Double)
                           }
             | QuickBar1 { elemScale :: Double  -- ^ Default: 1
                         , elemNumVisible :: Integer -- ^ Default: 12
                         , elemNumPerRow :: Integer -- ^ Default: 12
                         , elemBGVisible :: Bool  -- ^ Default: True
                         , elemOffset :: (Double, Double)
                         }
             | QuickBar2 { elemScale :: Double  -- ^ Default: 1
                         , elemNumVisible :: Integer -- ^ Default: 12
                         , elemNumPerRow :: Integer -- ^ Default: 12
                         , elemBGVisible :: Bool  -- ^ Default: True
                         , elemOffset :: (Double, Double)
                         }
             | QuickBar3 { elemScale :: Double  -- ^ Default: 1
                         , elemNumVisible :: Integer -- ^ Default: 12
                         , elemNumPerRow :: Integer -- ^ Default: 12
                         , elemBGVisible :: Bool  -- ^ Default: True
                         , elemOffset :: (Double, Double)
                         }
             | QuickBar4 { elemScale :: Double  -- ^ Default: 1
                         , elemNumVisible :: Integer -- ^ Default: 12
                         , elemNumPerRow :: Integer -- ^ Default: 12
                         , elemBGVisible :: Bool  -- ^ Default: True
                         , elemOffset :: (Double, Double)
                         }
             | QuickBar5 { elemScale :: Double  -- ^ Default: 1
                         , elemNumVisible :: Integer -- ^ Default: 12
                         , elemNumPerRow :: Integer -- ^ Default: 12
                         , elemBGVisible :: Bool  -- ^ Default: True
                         , elemOffset :: (Double, Double)
                         }
             | QuickBar6 { elemScale :: Double  -- ^ Default: 1
                         , elemNumVisible :: Integer -- ^ Default: 12
                         , elemNumPerRow :: Integer -- ^ Default: 12
                         , elemBGVisible :: Bool  -- ^ Default: True
                         , elemOffset :: (Double, Double)
                         }
             | SocialCenter { elemScale :: Double  -- ^ Default: 1
                            , elemOffset :: (Double, Double)
                            }
             | SocialNotifications { elemScale :: Double  -- ^ Default: 1
                                   , elemOffset :: (Double, Double)
                                   }
             | SystemMessages { elemScale :: Double  -- ^ Default: 0.8
                              , elemOffset :: (Double, Double)
                              }
             | TargetCastbar { elemScale :: Double  -- ^ Default: 1
                             , elemFlipHorizontal :: Bool  -- ^ Default: False
                             , elemOffset :: (Double, Double)
                             }
             | TargetFrame { elemScale :: Double  -- ^ Default: 1
                           , elemFlipHorizontal :: Bool  -- ^ Default: True
                           , elemEffectsOnBottom :: Bool  -- ^ Default: False
                           , elemDisplayWithNoTarget :: Bool  -- ^ default: True
                           , elemShowInfoText :: Bool  -- ^ Default: False
                           , elemShowRole :: Bool  -- ^ Default: True
                           , elemBuffScale :: Double  -- ^ Default: 0.6
                           , elemDebuffScale :: Double  -- ^ Default: 0.6
                           , elemShowPersonalHighlightBuffs :: Bool  -- ^ Default: False
                           , elemPersonalHighlightBuffsMaxDuration :: Double  -- ^ Default: 300
                           , elemShowPersonalHighlightDebuffs :: Bool  -- ^ Default: False
                           , elemPersonalHighlightDebuffsMaxDuration :: Double -- ^ Default: 300
                           , elemShowPersonalBuffsFirst :: Bool  -- ^ Default: False
                           , elemBuffsSortType :: EffectSortType  -- ^ Default: ApplyTime
                           , elemShowPersonalDebuffsFirst :: Bool  -- ^ Default: False
                           , elemDebuffsSortType :: EffectSortType  -- ^ Default: ApplyTime
                           , elemOffset :: (Double, Double)
                           }
             | TargetOfTarget { elemScale :: Double  -- ^ Default: 1
                              , elemFlipHorizontal :: Bool  -- ^ Default: True
                              , elemShowInfoText :: Bool  -- ^ Default: False
                              , elemEffectsOnBottom :: Bool  -- ^ Default: False
                              , elemShowRole :: Bool  -- ^ Default: True
                              , elemBuffScale :: Double  -- ^ Default: 0.25
                              , elemDebuffScale :: Double  -- ^ Default: 0.25
                              , elemOffset :: (Double, Double)
                              }
             | TargetOfTargetCastbar { elemScale :: Double -- ^ Default: 0.6
                                     , elemFlipHorizontal :: Bool  -- ^ Default: True
                                     , elemOffset :: (Double, Double)
                                     }
             | TemporaryAbilityBar { elemScale :: Double -- ^ Default: 1
                                   , elemOffset :: (Double, Double)
                                   }
             | Tutorials { elemScale :: Double -- ^ Default: 1
                         , elemOffset :: (Double, Double)
                         }
  deriving (Eq, Ord, Show, Read)

globals :: Globals
globals = Globals{ globScale = 1 }

-- | An alias for 'Anchor'
(<->) :: Alignment -> Alignment -> Element -> [LayoutPrim] -> LayoutPrim
(<->) = Anchor

-- | Get the first parent alignment found within a 'LayoutPrim'
layoutPrimParentAlignment :: LayoutPrim -> Maybe Alignment
layoutPrimParentAlignment Anchor{..} = Just lyParentAlign
layoutPrimParentAlignment Box{..} =
  asum (map layoutPrimParentAlignment (lyInside ++ lyChildren))

achievementTracker, cartelMarket, chatPanel1, chatPanel2, chatPanel3
  , chatPanel4, chatPanel5, chatPanel6, chatPanel7, chatPanel8
  , chatPanel9, chatPanel10, combatState, companion, companionCastBar
  , experienceBars, focusTarget, focusTargetCastbar, gameDownload
  , groupFrame1, groupFrame2, groupFrame3, groupTargetFrame1, groupTargetFrame2
  , groupTargetFrame3, holocom, largeTooltip, menuBar, miniMap, missionTracker
  , operationFrames, phaseIndicator, playerBuffTray, playerCastbar
  , playerDebuffTray, playerFrame, quickBar1, quickBar2, quickBar3, quickBar4
  , quickBar5, quickBar6, socialCenter, socialNotifications, systemMessages
  , targetCastbar, targetFrame, targetOfTarget, targetOfTargetCastbar
  , temporaryAbilityBar, tutorials :: Element

achievementTracker = AchievementTracker{ elemScale = 1
                                       , elemFlipVertical = True
                                       , elemHeight = 295
                                       , elemOffset = (0,0)
                                       }
cartelMarket = CartelMarket{ elemScale = 1
                           , elemOffset = (0,0)
                           }
chatPanel1  = ChatPanel1{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel2  = ChatPanel2{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel3  = ChatPanel3{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel4  = ChatPanel4{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel5  = ChatPanel5{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel6  = ChatPanel6{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel7  = ChatPanel7{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel8  = ChatPanel8{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel9  = ChatPanel9{ elemSize = (400,200)
                        , elemFontSize = 14
                        , elemOffset = (0,0)
                        }
chatPanel10 = ChatPanel10{ elemSize = (400,200)
                         , elemFontSize = 14
                         , elemOffset = (0,0)
                         }
combatState = CombatState{ elemScale = 1
                         , elemOffset = (0,0)
                         }
companion = Companion{ elemScale = 1
                     , elemOffset = (0,0)
                     }
companionCastBar = CompanionCastbar{ elemScale = 1
                                   , elemFlipHorizontal = False
                                   , elemOffset = (0,0)
                                   }
experienceBars = ExperienceBars{ elemScale = 1
                               , elemShowXP = True
                               , elemShowLegacyXP = True
                               , elemOffset = (0,0)
                               }
focusTarget = FocusTarget{ elemScale = 0.8
                         , elemFlipHorizontal = False
                         , elemShowInfoText = False
                         , elemBuffScale = 0.6
                         , elemDebuffScale = 0.6
                         , elemShowPersonalHighlightBuffs = False
                         , elemPersonalHighlightBuffsMaxDuration = 300
                         , elemShowPersonalHighlightDebuffs = False
                         , elemPersonalHighlightDebuffsMaxDuration = 300
                         , elemShowPersonalBuffsFirst = False
                         , elemBuffsSortType = ApplyTime
                         , elemShowPersonalDebuffsFirst = False
                         , elemDebuffsSortType = ApplyTime
                         , elemOffset = (0,0)
                         }
focusTargetCastbar = FocusTargetCastbar{ elemScale = 0.9
                                       , elemFlipHorizontal = False
                                       , elemOffset = (0,0)
                                       }
gameDownload = GameDownload{ elemScale = 1
                           , elemOffset = (0,0)
                           }
groupFrame1 = GroupFrame1{ elemScale = 1
                         , elemFlipHorizontal = False
                         , elemShowInfoText = False
                         , elemShowRole = True
                         , elemBuffScale = 0.35
                         , elemDebuffScale = 0.35
                         , elemOffset = (0,0)
                         }
groupFrame2 = GroupFrame2{ elemScale = 1
                         , elemFlipHorizontal = False
                         , elemShowInfoText = False
                         , elemShowRole = True
                         , elemBuffScale = 0.35
                         , elemDebuffScale = 0.35
                         , elemOffset = (0,0)
                         }
groupFrame3 = GroupFrame3{ elemScale = 1
                         , elemFlipHorizontal = False
                         , elemShowInfoText = False
                         , elemShowRole = True
                         , elemBuffScale = 0.35
                         , elemDebuffScale = 0.35
                         , elemOffset = (0,0)
                         }
groupTargetFrame1 = GroupTargetFrame1{ elemScale = 0.75
                                     , elemFlipHorizontal = True
                                     , elemShowInfoText = False
                                     , elemEffectsOnBottom = False
                                     , elemShowRole = True
                                     , elemBuffScale = 0.25
                                     , elemDebuffScale = 0.25
                                     , elemOffset = (0,0)
                                     }
groupTargetFrame2 = GroupTargetFrame2{ elemScale = 0.75
                                     , elemFlipHorizontal = True
                                     , elemShowInfoText = False
                                     , elemEffectsOnBottom = False
                                     , elemShowRole = True
                                     , elemBuffScale = 0.25
                                     , elemDebuffScale = 0.25
                                     , elemOffset = (0,0)
                                     }
groupTargetFrame3 = GroupTargetFrame3{ elemScale = 0.75
                                     , elemFlipHorizontal = True
                                     , elemShowInfoText = False
                                     , elemEffectsOnBottom = False
                                     , elemShowRole = True
                                     , elemBuffScale = 0.25
                                     , elemDebuffScale = 0.25
                                     , elemOffset = (0,0)
                                     }
holocom = Holocom{ elemScale = 1
                 , elemOffset = (0,0)
                 }
largeTooltip = LargeTooltip{ elemScale = 1.0
                           , elemAttachToMiniMap = True
                           , elemGrowUp = True
                           , elemOffset = (0,0)
                           }
menuBar = MenuBar{ elemScale = 1
                 , elemNumPerRow = 13
                 , elemOffset = (0,0)
                 }
miniMap = MiniMap{ elemScale = 0.9
                 , elemFlipVertical = False
                 , elemOffset = (0,0)
                 }
missionTracker = MissionTracker{ elemScale = 1
                               , elemFlipVertical = False
                               , elemHeight = 500
                               , elemOffset = (0,0)
                               }
operationFrames = OperationFrames{ elemScale = 1
                                 , elemNumPerRow = 3
                                 , elemNumVisible = 6
                                 , elemHealthSize = (102,4)
                                 , elemShowHealth = False
                                 , elemBuffScale = 0.22
                                 , elemDebuffScale = 0.22
                                 , elemPartySpacing = 0
                                 , elemShowOnlyCleansableDebuffs = False
                                 , elemOffset = (0,0)
                                 }
phaseIndicator = PhaseIndicator{ elemScale = 1
                               , elemOffset = (0,0)
                               }
playerBuffTray = PlayerBuffTray{ elemScale = 1
                               , elemIconScale = 0.6
                               , elemNumPerRow = 5
                               , elemExpandVertical = ExpandUp
                               , elemExpandHorizontal = ExpandRight
                               , elemBuffsSortType = ApplyTime
                               , elemShowPersonalHighlightBuffs = False
                               , elemPersonalHighlightBuffsMaxDuration = 300
                               , elemShowPersonalBuffsFirst = False
                               , elemOffset = (0,0)
                               }
playerCastbar = PlayerCastbar{ elemScale = 1
                             , elemFlipHorizontal = True
                             , elemOffset = (0,0)
                             }
playerDebuffTray = PlayerDebuffTray{ elemScale = 1
                                   , elemIconScale = 0.6
                                   , elemNumPerRow = 5
                                   , elemExpandVertical = ExpandUp
                                   , elemExpandHorizontal = ExpandLeft
                                   , elemDebuffsSortType = ApplyTime
                                   , elemShowPersonalHighlightDebuffs = False
                                   , elemPersonalHighlightDebuffsMaxDuration = 300
                                   , elemShowPersonalDebuffsFirst = False
                                   , elemOffset = (0,0)
                                   }
playerFrame = PlayerFrame{ elemScale = 1
                         , elemFlipHorizontal = False
                         , elemShowInfoText = False
                         , elemEffectsOnBottom = False
                         , elemShowRole = True
                         , elemOffset = (0,0)
                         }
quickBar1 = QuickBar1{ elemScale = 1
                     , elemNumVisible = 12
                     , elemNumPerRow = 12
                     , elemBGVisible = True
                     , elemOffset = (0,0)
                     }
quickBar2 = QuickBar2{ elemScale = 1
                     , elemNumVisible = 12
                     , elemNumPerRow = 12
                     , elemBGVisible = True
                     , elemOffset = (0,0)
                     }
quickBar3 = QuickBar3{ elemScale = 1
                     , elemNumVisible = 12
                     , elemNumPerRow = 12
                     , elemBGVisible = True
                     , elemOffset = (0,0)
                     }
quickBar4 = QuickBar4{ elemScale = 1
                     , elemNumVisible = 12
                     , elemNumPerRow = 12
                     , elemBGVisible = True
                     , elemOffset = (0,0)
                     }
quickBar5 = QuickBar5{ elemScale = 1
                     , elemNumVisible = 12
                     , elemNumPerRow = 12
                     , elemBGVisible = True
                     , elemOffset = (0,0)
                     }
quickBar6 = QuickBar6{ elemScale = 1
                     , elemNumVisible = 12
                     , elemNumPerRow = 12
                     , elemBGVisible = True
                     , elemOffset = (0,0)
                     }
socialCenter = SocialCenter{ elemScale = 1
                           , elemOffset = (0,0)
                           }
socialNotifications = SocialNotifications{ elemScale = 1
                                         , elemOffset = (0,0)
                                         }
systemMessages = SystemMessages{ elemScale = 0.8
                               , elemOffset = (0,0)
                               }
targetCastbar = TargetCastbar{ elemScale = 1
                             , elemFlipHorizontal = False
                             , elemOffset = (0,0)
                             }
targetFrame = TargetFrame{ elemScale = 1
                         , elemFlipHorizontal = True
                         , elemEffectsOnBottom = False
                         , elemDisplayWithNoTarget = True
                         , elemShowInfoText = False
                         , elemShowRole = True
                         , elemBuffScale = 0.6
                         , elemDebuffScale = 0.6
                         , elemShowPersonalHighlightBuffs = False
                         , elemPersonalHighlightBuffsMaxDuration = 300
                         , elemShowPersonalHighlightDebuffs = False
                         , elemPersonalHighlightDebuffsMaxDuration = 300
                         , elemShowPersonalBuffsFirst = False
                         , elemBuffsSortType = ApplyTime
                         , elemShowPersonalDebuffsFirst = False
                         , elemDebuffsSortType = ApplyTime
                         , elemOffset = (0,0)
                         }
targetOfTarget = TargetOfTarget{ elemScale = 1
                               , elemFlipHorizontal = True
                               , elemShowInfoText = False
                               , elemEffectsOnBottom = False
                               , elemShowRole = True
                               , elemBuffScale = 0.25
                               , elemDebuffScale = 0.25
                               , elemOffset = (0,0)
                               }
targetOfTargetCastbar = TargetOfTargetCastbar{ elemScale = 0.6
                                             , elemFlipHorizontal = True
                                             , elemOffset = (0,0)
                                             }
temporaryAbilityBar = TemporaryAbilityBar{ elemScale = 1
                                         , elemOffset = (0,0)
                                         }
tutorials = Tutorials{ elemScale = 1
                     , elemOffset = (0,0)
                     }

elementSize :: Element -> (Double, Double)
elementSize AchievementTracker{..} = scaleBy elemScale (258, elemHeight)
elementSize CartelMarket{..} = scaleBy elemScale (90, 41)
elementSize ChatPanel1{..} = elemSize
elementSize ChatPanel2{..} = elemSize
elementSize ChatPanel3{..} = elemSize
elementSize ChatPanel4{..} = elemSize
elementSize ChatPanel5{..} = elemSize
elementSize ChatPanel6{..} = elemSize
elementSize ChatPanel7{..} = elemSize
elementSize ChatPanel8{..} = elemSize
elementSize ChatPanel9{..} = elemSize
elementSize ChatPanel10{..} = elemSize
elementSize CombatState{..} = scaleBy elemScale (44, 40)
elementSize Companion{..} = scaleBy elemScale (395, 115)
elementSize CompanionCastbar{..} = scaleBy elemScale (262, 30)
elementSize ExperienceBars{..} = scaleBy elemScale (668, 12 * fromIntegral rows)
  where rows = (length . filter id) [elemShowXP, elemShowLegacyXP]
elementSize FocusTarget{..} = scaleBy elemScale (444, 136)
elementSize FocusTargetCastbar{..} = scaleBy elemScale (262, 30)
elementSize GameDownload{..} = scaleBy elemScale (250, 100)
elementSize GroupFrame1{..} = scaleBy elemScale (260, 123)
elementSize GroupFrame2{..} = scaleBy elemScale (260, 123)
elementSize GroupFrame3{..} = scaleBy elemScale (260, 123)
elementSize GroupTargetFrame1{..} = scaleBy elemScale (218, 70)
elementSize GroupTargetFrame2{..} = scaleBy elemScale (218, 70)
elementSize GroupTargetFrame3{..} = scaleBy elemScale (218, 70)
elementSize Holocom{..} = scaleBy elemScale (126, 206)
elementSize LargeTooltip{..} = scaleBy elemScale (294, 36)
elementSize MenuBar{..} = scaleBy elemScale (w, h)
  where (cols, rows) = dimensions 13 elemNumPerRow
        w = if cols == 1 then 48 else 33 + 43 * fromInteger cols
        h | rows == 1 = 33
          | otherwise = (if cols == 1 then 18 else 4) + 34 * fromInteger rows
elementSize MiniMap{..} = scaleBy elemScale (248, 238)
elementSize MissionTracker{..} = scaleBy elemScale (258, elemHeight)
elementSize OperationFrames{..} = scaleBy elemScale (w, h)
  where
    (cols, rows) = dimensions elemNumVisible elemNumPerRow
    w = fromInteger cols * (13 + fst elemHealthSize + elemPartySpacing)
      + elemPartySpacing
    h = fromInteger rows * 4 * (41 + snd elemHealthSize + elemPartySpacing)
      + elemPartySpacing + 24
elementSize PhaseIndicator{..} = scaleBy elemScale (101, 20)
elementSize PlayerBuffTray{..} = scaleBy (elemScale * elemIconScale) (48, 48)
elementSize PlayerCastbar{..} = scaleBy elemScale (262, 30)
elementSize PlayerDebuffTray{..} = scaleBy (elemScale * elemIconScale) (48, 48)
elementSize PlayerFrame{..} = scaleBy elemScale (444, 136)
elementSize QuickBar1{..} = quickBarSize 1 elemScale elemNumVisible elemNumPerRow
elementSize QuickBar2{..} = quickBarSize 2 elemScale elemNumVisible elemNumPerRow
elementSize QuickBar3{..} = quickBarSize 3 elemScale elemNumVisible elemNumPerRow
elementSize QuickBar4{..} = quickBarSize 4 elemScale elemNumVisible elemNumPerRow
elementSize QuickBar5{..} = quickBarSize 5 elemScale elemNumVisible elemNumPerRow
elementSize QuickBar6{..} = quickBarSize 6 elemScale elemNumVisible elemNumPerRow
elementSize SocialCenter{..} = scaleBy elemScale (300, 25)
elementSize SocialNotifications{..} = scaleBy elemScale (400, 32)
elementSize SystemMessages{..} = scaleBy elemScale (430, 85)
elementSize TargetCastbar{..} = scaleBy elemScale (262, 30)
elementSize TargetFrame{..} = scaleBy elemScale (444, 136)
elementSize TargetOfTarget{..} = scaleBy elemScale (218, 70)
elementSize TargetOfTargetCastbar{..} = scaleBy elemScale (262, 30)
elementSize TemporaryAbilityBar{..} = scaleBy elemScale (648, 66)
elementSize Tutorials{..} = scaleBy elemScale (63, 60)

quickBarSize :: Integer -> Double -> Integer -> Integer -> (Double, Double)
quickBarSize qbNumber scale numVisible numPerRow = scaleBy scale (w, h)
  where (cols, rows) = dimensions numVisible numPerRow
        w = (if qbNumber == 1 then 48 else 12) + 53 * fromInteger cols
        h = 13 + 53 * fromInteger rows

scaleBy :: Double -> (Double, Double) -> (Double, Double)
scaleBy scale (x, y) = (scale * x, scale * y)

dimensions :: Integer -> Integer -> (Integer, Integer)
dimensions numVisible numPerRow = (cols, rows)
  where cols = numPerRow
        rows = (numVisible + numPerRow - 1) `div` numPerRow
