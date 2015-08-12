{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PatternGuards #-}

import XMonad hiding ((|||))

import System.Exit

import qualified XMonad.StackSet as W
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

main :: IO ()
main = xmonad .
    ewmh $ --adds support of EWMH functions, makes touchegg work
    rossConfig




----------------------------------------------------------------------
-- config itself
----------------------------------------------------------------------
unfocusedColour, secondaryColour, highlightColour ∷ String
unfocusedColour = "#303030"
secondaryColour = "#707070"
highlightColour = "#57C7FF"


rossConfig = defaultConfig {
    terminal           = "lxterminal",
    modMask            = mod4Mask,
    workspaces         = myWorkspaces,
    borderWidth        = 1,
    normalBorderColor  = unfocusedColour,
    focusedBorderColor = highlightColour,
    focusFollowsMouse  = False,
    manageHook         = myManageHook <+> manageDocks,
    handleEventHook    = docksEventHook,
    logHook            = myLogHook,
    layoutHook         = myLayouts,
    keys               = myKeyBindings,
    XMonad.clickJustFocuses = False
}

------------------------------------------------------------------------
-- workspaces
------------------------------------------------------------------------

numberOfWorkspaces :: Int
numberOfWorkspaces = 6

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. numberOfWorkspaces]








------------------------------------------------------------------------
-- keybindings
------------------------------------------------------------------------

myKeyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeyBindings c = mkKeymap c (rawKeys c)

rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = concatMap ($ c) keymaps where
    keymaps = [baseKeys, spawnKeys, fKeys, focusKeys, mediaKeys]

baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ =
    [ ("M-h", spawn "hibernate")
    , ("M-<Escape>", kill)
    , ("M-C-r", spawn "/home/ross/.scripts/xmonad_recompile")
    , ("M-C-S-r", restart "/home/ross/.xmonad/xmonad" True)
    , ("M-d", sendMessage ToggleStruts)
    , ("M-C-S-<Escape>", io exitSuccess)
    ]

spawnKeys _ =
    [ ("M-<Return>", spawn "urxvt")
    , ("M-a", spawn "atom")
    , ("M-b", spawn "/home/ross/.scripts/bluetooth_connect")
    , ("M-c", spawn "gnome-calculator")
    , ("M-e", spawn "thunar")
    , ("M-f", spawn "firefox-aurora")
    , ("M-h", spawn "hibernate")
    , ("M-l", spawn "/home/ross/.scripts/lock_screen")
    , ("M-s", spawn "sleep")
    , ("M-r", spawn "firefox-aurora -P Rdio -no-remote")
    -- , ("M-u", spawn "/home/ross/.scripts/keyboard_setup_udev")
    , ("M-v", spawn "pavucontrol")
    -- Toggle Keyboard mapping
    , ("M-k", spawn "xmodmap /home/ross/.xmodmap_braces && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Braces Activated\"")
    , ("M-S-k", spawn "xmodmap /home/ross/.xmodmap_sbrackets && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Square Brackets Activated\"")
    , ("M-j", spawn "xmodmap /home/ross/.xmodmap_symbols && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Symbols Activated\"")
    , ("M-S-j", spawn "xmodmap /home/ross/.xmodmap_numbers && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Numbers Activated\"")
    ]

fKeys _ =
    [ ("<F1>", spawn "sudo chvt 2")
    , ("<F2>", spawn "/home/ross/.scripts/brightness down")
    , ("<F3>", spawn "/home/ross/.scripts/brightness up")
    , ("<F4>", spawn "/home/ross/.scripts/screens")
    , ("<F6>", spawn "/home/ross/.scripts/volume toggle")
    , ("S-<F6>", spawn "sudo /home/ross/.scripts/bluetooth-connect")
    , ("<F7>", spawn "/home/ross/.scripts/volume down")
    , ("<F8>", spawn "/home/ross/.scripts/volume up")
    , ("S-<F6>", spawn "/home/ross/.scripts/music_control play")
    , ("S-<F7>", spawn "/home/ross/.scripts/music_control prev")
    , ("S-<F8>", spawn "/home/ross/.scripts/music_control next")
    , ("<F9>", spawn "sudo /home/ross/.scripts/keyboard_backlight down")
    , ("<F10>", spawn "sudo /home/ross/.scripts/keyboard_backlight up")
    , ("<F11>", spawn "samsung-tools -c cycle && notify-send \"$(samsung-tools -c status)\"")
    , ("<F12>", spawn "samsung-tools -W toggle && notify-send -i /usr/share/icons/gnome/48x48/devices/network-wireless.png \"$(samsung-tools -W status)\"")
    , ("S-<F12>", spawn "samsung-tools -B toggle && notify-send -i /usr/share/icons/gnome/scalable/apps/bluetooth-symbolic.svg \"$(samsung-tools -B status)\"")
    ]
    ++
    [ ("M-<F" ++ show n ++ ">", spawn $ "sleep 0.2 && xdotool key --clearmodifiers --window $(xdotool getactivewindow) F" ++ show n) | n <- [1..12::Int]]

mediaKeys _ =
    [ ("<XF86AudioPlay>", spawn "/home/ross/.scripts/music_control play")
    , ("<XF86AudioNext>", spawn "/home/ross/.scripts/music_control next")
    , ("<XF86AudioPrev>", spawn "/home/ross/.scripts/music_control prev")
    , ("<XF86AudioMute>", spawn "/home/ross/.scripts/volume toggle")
    , ("<XF86AudioLowerVolume>", spawn "/home/ross/.scripts/volume down")
    , ("<XF86AudioRaiseVolume>", spawn "/home/ross/.scripts/volume up")
    ]

focusKeys c =
    [
    -- moving focus
    ("M-<L>", windows W.focusUp)
    , ("M-<R>", windows W.focusDown)
    , ("M-<Space>", windows W.focusMaster)
    , ("M-<Backspace>", swapNextScreen)
    , ("M-S-<Backspace>", nextScreen)
    , ("M-n", moveTo Next EmptyWS)
    , ("M-<Home>", moveTo Prev NonEmptyWS)
    , ("M-<End>", moveTo Next NonEmptyWS)
    , ("M-<Tab>", toggleWS)
    -- , ("M-<Backspace>", windows $ showSpare)
    -- moving windows
    , ("M-C-<L>", windows W.swapUp)
    , ("M-C-<R>", windows W.swapDown)
    , ("M-C-<Space>", windows W.swapMaster)
    , ("M-C-<Backspace>", shiftNextScreen)
    , ("M-C-n",  shiftTo Next EmptyWS)
    , ("M-C-<Home>", shiftTo Prev NonEmptyWS >> moveTo Prev NonEmptyWS)
    , ("M-C-<End>", shiftTo Next NonEmptyWS >> moveTo Next NonEmptyWS)
    -- fullscreen and floating
    , ("M-<U>", withFocused $ windows . (\w -> W.float w (W.RationalRect 0.0 0.0 1.0 1.0)))
    , ("M-<D>", withFocused $ windows . W.sink)
    -- change layout
    , ("M-,", sendMessage Shrink)
    , ("M-.", sendMessage Expand)
    , ("M-~", sendMessage NextLayout)
    , ("M-S-,", sendMessage (IncMasterN (-1)))
    , ("M-S-.", sendMessage (IncMasterN 1))
    ]
    ++
    [("M-" ++ show k, windows $ W.greedyView i) | (k, i) <- zip [1..numberOfWorkspaces] (XMonad.workspaces c)]
    ++
    [("M-C-" ++ show k, windows $ W.greedyView i . W.shift i) | (k, i) <- zip [1..numberOfWorkspaces] (XMonad.workspaces c)]
    ++
    [("M-S-C-" ++ show k, windows $ W.shift i) | (k, i) <- zip [1..numberOfWorkspaces] (XMonad.workspaces c)]












------------------------------------------------------------------------
-- window rules
------------------------------------------------------------------------

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Xmessage"  --> doFloat
    ]















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
        spacing 5 $
        tiled ||| Mirror tiled ||| Full
    where
        tiled = Tall 1 (3 / 100) (1 / 2)
