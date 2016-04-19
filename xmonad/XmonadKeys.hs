{-# LANGUAGE UnicodeSyntax #-}

module XmonadKeys ( myKeyBindings ) where

import XMonad hiding ((|||))

import XmonadConfig

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.ManageDocks

import XMonad.Actions.CycleWS
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

------------------------------------------------------------------------
-- keybindings
------------------------------------------------------------------------

myKeyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeyBindings c = mkKeymap c (rawKeys c)

rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = concatMap ($ c) keymaps where
    keymaps = [baseKeys, spawnKeys, scratches, fKeys, focusKeys, mediaKeys]

baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ =
    [ ("M-h", spawn "hibernate")
    -- , ("M-s", spawn "sleep")
    , ("M-<Escape>", kill)
    , ("M-C-r", spawn "/home/ross/.scripts/xmonad_recompile")
    , ("M-C-S-r", spawn "pkill -fx /home/ross/.local/bin/xmonad-x86_64-linux")
    , ("M-d", sendMessage ToggleStruts)
    , ("M-C-S-<Escape>", io exitSuccess)
    ]

spawnKeys _ =
    [ ("M-<Return>", spawn myTerminal)
    , ("M-a", spawn "atom")
    , ("M-b", spawn "/home/ross/.scripts/bluetooth_connect")
    , ("M-c", spawn "gnome-calculator")
    , ("M-e", spawn "thunar")
    , ("M-f", spawn "firefox-aurora")
    , ("M-l", spawn "/home/ross/.scripts/lock_screen")
    , ("M-r", spawn "firefox-aurora -P Rdio -no-remote")
    , ("M-s", spawn "spotify")
    -- , ("M-u", spawn "/home/ross/.scripts/keyboard_setup_udev")
    , ("M-m", spawn "/home/ross/.scripts/eris mount && notify-send -i /usr/share/icons/gnome/48x48/devices/drive-multidisk.png \"Eris Mounted\"")
    , ("M-S-m", spawn "/home/ross/.scripts/eris unmount && notify-send -i /usr/share/icons/elementary/actions/48/remove.svg \"Eris Unmounted\"")
    , ("M-v", spawn "pavucontrol")
    -- Toggle Keyboard mapping
    , ("M-k", spawn "xmodmap /home/ross/.xmodmap_braces && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Braces Activated\"")
    , ("M-S-k", spawn "xmodmap /home/ross/.xmodmap_sbrackets && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Square Brackets Activated\"")
    , ("M-j", spawn "xmodmap /home/ross/.xmodmap_symbols && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Symbols Activated\"")
    , ("M-S-j", spawn "xmodmap /home/ross/.xmodmap_numbers && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Numbers Activated\"")
    ]

scratches _ =
    [ ("M-<Space>", namedScratchpadAction scratchpads "clock")
    ]

fKeys _ =
    [ ("<F1>", spawn "/home/ross/.scripts/battery")
    , ("<F2>", spawn "/home/ross/.scripts/brightness down")
    , ("<F3>", spawn "/home/ross/.scripts/brightness up")
    , ("<F4>", spawn "/home/ross/.scripts/screens")
    , ("S-<F4>", spawn "/home/ross/.scripts/screens off")
    , ("<F6>", spawn "/home/ross/.scripts/volume toggle")
    , ("S-<F6>", spawn "sudo /home/ross/.scripts/bluetooth-connect")
    , ("<F7>", spawn "/home/ross/.scripts/volume down")
    , ("<F8>", spawn "/home/ross/.scripts/volume up")
    , ("S-<F6>", spawn "/home/ross/.scripts/music-control play")
    , ("S-<F7>", spawn "/home/ross/.scripts/music-control prev")
    , ("S-<F8>", spawn "/home/ross/.scripts/music-control next")
    , ("<F9>", spawn "sudo /home/ross/.scripts/keyboard_backlight down")
    , ("<F10>", spawn "sudo /home/ross/.scripts/keyboard_backlight up")
    , ("<F11>", spawn "samsung-tools -c cycle && notify-send \"$(samsung-tools -c status)\"")
    , ("<F12>", spawn "samsung-tools -W toggle && notify-send -i /usr/share/icons/gnome/48x48/devices/network-wireless.png \"$(samsung-tools -W status)\"")
    , ("S-<F12>", spawn "samsung-tools -B toggle && notify-send -i /usr/share/icons/gnome/scalable/apps/bluetooth-symbolic.svg \"$(samsung-tools -B status)\"")
    ]
    ++
    [ ("M-<F" ++ show n ++ ">", spawn $ "sleep 0.2 && xdotool key --clearmodifiers --window $(xdotool getactivewindow) F" ++ show n) | n <- [1..12::Int]]


mediaKeys _ =
    [ ("<XF86AudioPlay>", spawn "/home/ross/.scripts/music-control play")
    , ("<XF86AudioNext>", spawn "/home/ross/.scripts/music-control next")
    , ("<XF86AudioPrev>", spawn "/home/ross/.scripts/music-control prev")
    , ("<XF86AudioMute>", spawn "/home/ross/.scripts/volume toggle")
    , ("<XF86AudioLowerVolume>", spawn "/home/ross/.scripts/volume down")
    , ("<XF86AudioRaiseVolume>", spawn "/home/ross/.scripts/volume up")
    ]


focusKeys c =
    [
    -- moving focus
    ("M-<L>", windows W.focusUp)
    , ("M-<R>", windows W.focusDown)
    -- , ("M-<Space>", windows W.focusMaster)
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
    [("M-C-" ++ show k, windows $ W.shift i) | (k, i) <- zip [1..numberOfWorkspaces] (XMonad.workspaces c)]
    ++
    [("M-S-C-" ++ show k, windows $ W.greedyView i . W.shift i) | (k, i) <- zip [1..numberOfWorkspaces] (XMonad.workspaces c)]
