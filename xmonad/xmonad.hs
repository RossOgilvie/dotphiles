{-# LANGUAGE UnicodeSyntax #-}

import XmonadKeys (myKeyBindings)
import XmonadConfig

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W


import System.Exit
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
-- provides ewmh hooks for making touchegg work
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad


main :: IO ()
--adds support of EWMH functions, makes touchegg work
main = xmonad . ewmh $ rossConfig

----------------------------------------------------------------------
-- config itself
----------------------------------------------------------------------


rossConfig = defaultConfig {
    terminal           = myTerminal,
    modMask            = mod4Mask,
    workspaces         = myWorkspaces,
    borderWidth        = 1,
    normalBorderColor  = unfocusedColour,
    focusedBorderColor = highlightColour,
    focusFollowsMouse  = False,
    manageHook         = myManageHook <+> manageDocks <+> manageScratchPad,
    handleEventHook    = docksEventHook,
    logHook            = myLogHook,
    layoutHook         = myLayouts,
    keys               = myKeyBindings,
    XMonad.clickJustFocuses = False
}

------------------------------------------------------------------------
-- window rules
------------------------------------------------------------------------

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Xmessage"  --> doFloat
    ]


manageScratchPad :: ManageHook
manageScratchPad = namedScratchpadManageHook scratchpads

------------------------------------------------------------------------
-- status bar and logging
------------------------------------------------------------------------

-- no logging to a status bar
myLogHook = return ()

------------------------------------------------------------------------
-- layouts
------------------------------------------------------------------------

myLayouts = smartBorders $
        avoidStruts $
        showWName $
        spacing gapSize $
        tiled ||| Mirror tiled ||| Full
    where
        tiled = Tall 1 (3 / 100) (1 / 2)
