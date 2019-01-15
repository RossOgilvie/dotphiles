{-# LANGUAGE UnicodeSyntax #-}

import XmonadKeys (myKeyBindings, fnKeyActiveEventHook)
import XmonadConfig

-- import XMonad hiding ((|||))
import XMonad

-- provides ewmh hooks for making touchegg work
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.DynamicLog
-- import XMonad.Util.Run (spawnPipe)
import System.IO

-- import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.WindowArranger

import XMonad.Layout.Fullscreen
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
-- import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Dishes
import XMonad.Layout.TwoPane
-- import XMonad.Layout.Circle

import XMonad.Util.NamedScratchpad


main :: IO ()
--adds support of EWMH functions, makes touchegg work
main = do
    xmonad . ewmh $ rossConfig

----------------------------------------------------------------------
-- config itself
----------------------------------------------------------------------


rossConfig = def {
    terminal           = myTerminal,
    modMask            = myModKey,
    workspaces         = myWorkspaces,
    borderWidth        = myBorderWidth,
    normalBorderColor  = unfocusedColour,
    focusedBorderColor = highlightColour,
    focusFollowsMouse  = False,
    XMonad.clickJustFocuses = False,

    keys               = myKeyBindings,
    handleEventHook    = fnKeyActiveEventHook,
    logHook            = myLogHook,
    layoutHook         = myLayouts,
    manageHook         = myManageHook <+> manageDocks <+> manageScratchPad
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
--------------------------------------------------------------
-- myLayouts = smartBorders
--         . avoidStruts
--         . showWName
--         . spacing gapSize
--         $ tilde ||| spiral 1 ||| Full ||| Circle
--     where
--         tiled = Tall 1 (3 / 100) (1 / 2)

tall = ResizableTall 1 (2/100) (1/2) []
full = noBorders (fullscreenFull Full)
-- oneBig = OneBig (3/4) (3/4)

myLayouts = windowArrange layouts
-- layouts = (gaps [(U, 18 + 2), (R, gapSize), (L, gapSize), (D, gapSize)] $ avoidStruts (spacing gapSize $ res)) ||| Circle ||| ful
-- layouts = avoidStruts (spacing gapSize $ tall)  ||| full ||| TwoPane (3/100) (1/2) ||| emptyBSP ||| Dishes 2 (1/6)
layouts = avoidStruts (spacing gapSize $ tall)  ||| full ||| TwoPane (3/100) (1/2)
