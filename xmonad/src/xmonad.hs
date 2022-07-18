import qualified Data.List as L
import XMonad
import XMonad.Hooks.DynamicLog ()

import XMonad.Hooks.EwmhDesktops (
  ewmh,
  ewmhDesktopsLogHookCustom,
 )
-- provides ewmh hooks. These allow other programs to interact with desktop state, eg window lists, worsppace switching
import XMonad.Hooks.ManageDocks (avoidStruts) 
import XMonad.Hooks.ManageHelpers (
  doFullFloat,
  doRectFloat,
  isFullscreen,
 )

-- import XMonad.Layout.LayoutCombinators

-- import           XMonad.Layout.Gaps

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName (showWName)
import XMonad.Layout.Spacing
import XMonad.Layout.TwoPane

-- import XMonad.Layout.Accordion
-- import XMonad.Layout.BinarySpacePartition
-- import XMonad.Layout.Circle
-- import XMonad.Layout.Dishes
-- import XMonad.Layout.Spiral
import XMonad.Layout.WindowArranger (windowArrange)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XmonadConfig
import XmonadKeys (
  fnKeyActiveEventHook,
  myKeyBindings,
 )

main :: IO ()
main = xmonad rossConfig

----------------------------------------------------------------------
-- config itself
----------------------------------------------------------------------

rossConfig =
  ewmh $
    def
      { terminal = myTerminal
      , modMask = myModKey
      , workspaces = myWorkspaces
      , borderWidth = myBorderWidth
      , normalBorderColor = unfocusedColour
      , focusedBorderColor = highlightColour
      , focusFollowsMouse = False
      , XMonad.clickJustFocuses = False
      , keys = myKeyBindings
      , handleEventHook = fnKeyActiveEventHook
      , logHook = myLogHook
      , layoutHook = myLayouts
      , manageHook = myManageHook <+> manageScratchPad
      }

------------------------------------------------------------------------
-- window rules
------------------------------------------------------------------------

myManageHook =
  composeAll $
    [ isFullscreen --> doFullFloat
    , className =? "Xmessage" --> doFloat
    , className =? "eww" --> doFloat
    , className =? "eww" --> doIgnore
    , --   , className =? "vlc" --> doFullFloat
      stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doRectFloat (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4))
    ]

manageScratchPad :: ManageHook
manageScratchPad = namedScratchpadManageHook scratchpads

------------------------------------------------------------------------
-- status bar and logging
------------------------------------------------------------------------

-- The ewmh log hook supplies the window list to other programs
-- Filter out the NSP named scratchpad workspace
myLogHook = ewmhDesktopsLogHookCustom namedScratchpadFilterOutWorkspace

------------------------------------------------------------------------
-- layouts
--------------------------------------------------------------
layoutOpts =
  windowArrange . avoidStruts . smartBorders . standardSpacing
--   windowArrange . avoidStruts . showWName . smartBorders . standardSpacing

standardSpacing =
  spacingRaw
    True -- smartBorder, no borders on single window
    (Border gapSize gapSize gapSize gapSize) -- screen border
    True -- screen border enabled
    (Border gapSize gapSize gapSize gapSize) -- window border
    True -- window border enabled

tall = ResizableTall 1 (2 / 100) (1 / 2) []

full = noBorders (fullscreenFull Full)

-- oneBig = OneBig (3/4) (3/4)

myLayouts = layoutOpts $ tall ||| full ||| TwoPane (3 / 100) (1 / 2)
