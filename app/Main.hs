module Main (main) where

import SWTOR.UIProfile

main :: IO ()
main = do
  profilePath <- getProfilePath "ion"
  putStrLn ("Writing " ++ show profilePath)
  writeLayout profilePath layout

effectSort :: EffectSortType
effectSort = TotalDuration

layout :: Layout
layout =
  Screen globals{ globScale = 0.9 }
    [ (B <-> B) experienceBars
      [ (T <-> B) miniMap
        [ (BL <-> BR) quickBar2{ elemNumPerRow = 6, elemOffset = (-12, 0) }
          [ (T <-> B) quickBar1{ elemNumPerRow = 6 }
            [ (TL <-> BL) playerFrame{ elemShowInfoText = True, elemOffset = (-125, 47) }
              [ (TL <-> BL) playerBuffTray{ elemIconScale = 0.6
                                          , elemNumPerRow = 15
                                          , elemBuffsSortType = effectSort
                                          , elemShowPersonalHighlightBuffs = True
                                          , elemShowPersonalBuffsFirst = True
                                          , elemOffset = (118, 19)
                                          }
                [
                ]
                , (TL <-> B) holocom []
              ]
            ]
          ]
        , (BR <-> BL) quickBar4{ elemNumPerRow = 6, elemOffset = (-4, 0) }
          [ (T <-> B) quickBar3{ elemNumPerRow = 6 }
            [ (TR <-> BR) targetFrame{ elemShowInfoText = True
                                     , elemBuffScale = 0.6
                                     , elemDebuffScale = 0.8
                                     , elemShowPersonalHighlightBuffs = True
                                     , elemShowPersonalHighlightDebuffs = True
                                     , elemShowPersonalBuffsFirst = True
                                     , elemBuffsSortType = effectSort
                                     , elemShowPersonalDebuffsFirst = True
                                     , elemDebuffsSortType = effectSort
                                     , elemOffset = (125, 47)
                                     }
              []
            ]
          , (BR <-> BL) quickBar6{ elemScale = 0.9, elemNumPerRow = 6, elemBGVisible = False, elemOffset = (6, 0) }
            [ (T <-> B) quickBar5{ elemScale = 0.9, elemNumPerRow = 6, elemBGVisible = False, elemOffset = (0, 6) }
              []
            ]
          ]
        , (T <-> B) combatState{ elemOffset = (0, -30) }  -- There is text and stuff above the minimap, move this upwards.
          [ (T <-> B) tutorials
            [ (T <-> B) targetOfTarget{ elemShowInfoText = True
                                      , elemScale = 1.25
                                      , elemBuffScale = 0.25
                                      , elemDebuffScale = 0.4
                                      }
              [ (R <-> L) targetOfTargetCastbar{ elemScale = 1, elemFlipHorizontal = False, elemOffset = (-20, 0) } []
              , (T <-> B) temporaryAbilityBar []
              ]
            ]
          ]
        ]
      ]
    , (TR <-> TR) missionTracker{ elemScale = 0.9, elemHeight = 650/0.9 } []
    , (BR <-> BR) achievementTracker{ elemHeight = 650, elemOffset = (0, -24) } []
    , (BR <-> BR) largeTooltip{ elemAttachToMiniMap = False, elemOffset = (0, -100) } []
    , (C <-> BR) playerCastbar{ elemScale = 1.25, elemOffset = (-100, -100) }
      [ (BL <-> TL) playerDebuffTray{ elemScale = 1.25
                                    , elemIconScale = 0.8
                                    , elemNumPerRow = 5
                                    , elemExpandVertical = ExpandDown
                                    , elemExpandHorizontal = ExpandRight
                                    , elemDebuffsSortType = effectSort
                                    , elemShowPersonalHighlightDebuffs = True
                                    , elemShowPersonalDebuffsFirst = True
                                    , elemOffset = (0, 150)
                                    }
        []
      ]
    , (C <-> BL) targetCastbar{ elemScale = 1.25, elemOffset = ( 100, -100) }
      [ (R <-> L) focusTargetCastbar{ elemScale = 1.25, elemFlipHorizontal = False } []
      ]
    , (C <-> B) systemMessages{ elemScale = 1.25, elemOffset = (0, -250) } []
    , (BL <-> BL) companion{ elemOffset = (30, -60) }
      [ (R <-> L) companionCastBar{ elemScale = 1, elemOffset = (-80, -30) } []
      , (TL <-> BL) focusTarget{ elemScale = 0.85
                               , elemShowInfoText = True
                               , elemBuffScale = 0.6
                               , elemDebuffScale = 0.8
                               , elemShowPersonalHighlightBuffs = True
                               , elemShowPersonalHighlightDebuffs = True
                               , elemShowPersonalBuffsFirst = True
                               , elemBuffsSortType = effectSort
                               , elemShowPersonalDebuffsFirst = True
                               , elemDebuffsSortType = effectSort
                               }
        [ (TL <-> BL) groupFrame1{ elemScale = 1.25, elemShowInfoText = True, elemBuffScale = 0.4, elemDebuffScale = 0.5 }
          [ (R <-> L) groupTargetFrame1{ elemScale = 1.0, elemShowInfoText = True, elemBuffScale = 0.4, elemDebuffScale = 0.5 } []
          , (TL <-> BL) groupFrame2{ elemScale = 1.25, elemShowInfoText = True, elemBuffScale = 0.4, elemDebuffScale = 0.5 }
            [ (R <-> L) groupTargetFrame2{ elemScale = 1.0, elemShowInfoText = True, elemBuffScale = 0.4, elemDebuffScale = 0.5 } []
            , (TL <-> BL) groupFrame3{ elemScale = 1.25, elemShowInfoText = True, elemBuffScale = 0.4, elemDebuffScale = 0.5 }
              [ (R <-> L) groupTargetFrame3{ elemScale = 1.0, elemShowInfoText = True, elemBuffScale = 0.4, elemDebuffScale = 0.5 } []
              ]
            ]
          , (BL <-> BL) operationFrames{ elemScale = 1.25
                                       , elemBuffScale = 0.4
                                       , elemDebuffScale = 0.5
                                       , elemNumPerRow = 3
                                       , elemHealthSize = (160, 15)
                                       , elemShowHealth = True
                                       , elemShowOnlyCleansableDebuffs = True
                                       }
            []
          ]
        ]
      ]
    , (TL <-> TL) socialCenter
      [ (BL <-> TL) chatPanel2{ elemSize = (800, 190) }
        [ (BL <-> TL) chatPanel1{ elemSize = (800, 190) }
          [ (BL <-> TL) socialNotifications{ elemOffset = (30, 0) }
            []
          ]
        ]
      ]
    , (T <-> T) menuBar
      [ (TR <-> TL) cartelMarket []
      , (B <-> T) phaseIndicator []
      ]
    ]
