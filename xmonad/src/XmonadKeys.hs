{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}

module XmonadKeys ( myKeyBindings , fnKeyActiveEventHook) where

import XMonad hiding ((|||))

import XmonadConfig

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.ManageDocks

import XMonad.Actions.CycleWS
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import qualified XMonad.Util.ExtensibleState as XS
import qualified Graphics.X11.Types as X11
import Data.Monoid (All(..))

------------------------------------------------------------------------
-- Function Key Modality
------------------------------------------------------------------------

-- This data type is effectively a mutable singleton using ExtensibleState
data FnKeyActive = FnKeyActive { getFnKeyActive :: Bool } deriving (Typeable,Read,Show)
instance ExtensionClass FnKeyActive where
    initialValue = FnKeyActive True
    extensionType = PersistentExtension

-- toggles the bool value of FnKeyActive
-- Good to use as a key binding action
toggleFnKeyActive = XS.modify $ FnKeyActive . not . getFnKeyActive

-- An example of a function that only runs an action if the bool is True
fnKeyActiveIf :: X () -> X ()
fnKeyActiveIf act = do
    active <- XS.gets getFnKeyActive
    if active then act else return ()

-- An event hook that fights with Xmomad's processing of keypresses
-- Xmonad grabs all key presses that match its list of key bindings. They do not get passed through to the underlying window, even if the action performed is nothing (because how could Xmonad know nothing was happening)
-- The trick is to call "ungrabKey" from the X11 library to release Xmonad's grab and let the even pass through.
-- The return (All True) is the signal to continue processing with the normal hooks. I think that it should be All False in the case that we ungrab, but it seems to work this way.
fnKeyActiveEventHook :: Event -> X All
fnKeyActiveEventHook (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
    | t == keyPress = withDisplay $ \dpy -> do
        XConf { theRoot = rootw } <- ask
        s  <- io $ keycodeToKeysym dpy code 0
        let fns = [ X11.xK_F1, X11.xK_F2, X11.xK_F3, X11.xK_F4, X11.xK_F5, X11.xK_F6, X11.xK_F7, X11.xK_F8, X11.xK_F9, X11.xK_F10, X11.xK_F11, X11.xK_F12]
        active <- XS.gets getFnKeyActive
        
        -- if the key is in our list of fn keys, and fn keys are not active
        if s `elem` fns && not active 
            then -- ungrab the key so it proceeds to the underlying window
                io $ ungrabKey dpy code anyModifier rootw 
            else return ()
        return (All True)
    | otherwise = return (All True)
fnKeyActiveEventHook _ = return (All True)

------------------------------------------------------------------------
-- keybindings
------------------------------------------------------------------------

myKeyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeyBindings c = mkKeymap c (rawKeys c)

rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = concatMap ($ c) keymaps where
    keymaps = [baseKeys, spawnKeys, scratches, fKeys, functionKeys, focusKeys, mediaKeys]

baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ =
    [ ("M-h", spawn "hibernate")
    -- , ("M-s", spawn "sleep")
    , ("M-<Escape>", kill)
    , ("M-S-<Escape>", spawn "xkill")
    , ("M-C-r", do
        whenX (recompile True) $ do
            broadcastMessage ReleaseResources
            restart "/home/ross/.xmonad/xmonad-x86_64-linux" True)
    , ("M-d", sendMessage ToggleStruts)
    , ("M-t", toggleFnKeyActive)
    , ("M-C-S-<Escape>", io exitSuccess)
    ]

spawnKeys :: XConfig Layout -> [(String, X ())]
spawnKeys _ =
    [ ("M-<Return>", spawn myTerminal)
    , ("M-<Insert>", spawn "/home/ross/.scripts/screenshot")
    , ("M-<Print>", spawn "/home/ross/.scripts/screenshot")
    , ("M-w", spawn "mousepad")
    , ("M-b", spawn "/home/ross/.scripts/bluetooth_connect")
    , ("M-S-c", spawn "gnome-calculator")
    , ("M-S-e", spawn myFileBrowser)
    , ("M-f", spawn myWebBrowser)
    , ("M-<XF86Go>", spawn myLauncher)
    , ("M-s", spawn "spotify")
    , ("M-v", spawn "pavucontrol")

    -- , ("M-w", spawn "sudo /home/ross/.scripts/wifi toggle")
    , ("M-l", spawn "/home/ross/.scripts/lock-screen")
    , ("M-m", spawn "/home/ross/.scripts/dora mount && notify-send -i /usr/share/icons/gnome/48x48/devices/drive-multidisk.png \"Dora Mounted\"")
    , ("M-S-m", spawn "/home/ross/.scripts/dora unmount && notify-send -i /usr/share/icons/elementary/actions/48/remove.svg \"Dora Unmounted\"")
 
    -- , ("M-k", spawn "/home/ross/.scripts/keyboard setup notify")
    -- Pressing either bracket key flips it to the other one. Acts as a natural toggle.
    , ("M-{", spawn "/home/ross/.scripts/keyboard brackets notify")
    , ("M-}", spawn "/home/ross/.scripts/keyboard brackets notify")
    , ("M-[", spawn "/home/ross/.scripts/keyboard braces notify")
    , ("M-]", spawn "/home/ross/.scripts/keyboard braces notify")
    ]

scratches :: XConfig Layout -> [(String, X ())]
scratches _ =
    [ ("<XF86Go>", namedScratchpadAction scratchpads "conky")
    , ("M-e", namedScratchpadAction scratchpads "thunar")
    , ("M-c", namedScratchpadAction scratchpads "calc")
    ]

functionKeys :: XConfig Layout -> [(String, X ())]
functionKeys _ =
    -- F1
    [ ("<XF86Launch1>", spawn "/home/ross/.scripts/battery")
    -- F2
    , ("<XF86MonBrightnessDown>", spawn "/home/ross/.scripts/brightness down")
    -- F3
    , ("<XF86MonBrightnessUp>", spawn "/home/ross/.scripts/brightness up")
    -- F4
    , ("<XF86Display> <XF86Display>", spawn "/home/ross/.scripts/monitor auto")
    , ("<XF86Display> m", spawn "/home/ross/.scripts/monitor mirror")
    , ("<XF86Display> o", spawn "/home/ross/.scripts/monitor off")
    , ("<XF86Display> <Tab>", spawn "/home/ross/.scripts/monitor off")
    -- F5
    -- Use this to do the keyboard stuff instead
    , ("<XF86TouchpadOn>", spawn "/home/ross/.scripts/keyboard setup notify")
    , ("<XF86TouchpadOff>", spawn "/home/ross/.scripts/keyboard setup notify")
    -- F6-F8 are music keys, handled below in media keys
    -- F9
    , ("<XF86KbdBrightnessDown>", spawn "sudo /home/ross/.scripts/keyboard_backlight down")
    -- F10
    , ("<XF86KbdBrightnessUp>", spawn "sudo /home/ross/.scripts/keyboard_backlight up")
    -- F11
    , ("<XF86Launch3>", spawn "samsung-tools -c cycle && notify-send \"$(samsung-tools -c status)\"")
    -- F12
    , ("<XF86WLAN>", spawn "samsung-tools -W toggle && notify-send -i /usr/share/icons/gnome/48x48/devices/network-wireless.png \"$(samsung-tools -W status)\"")
    ]

fKeys :: XConfig Layout -> [(String, X ())]
fKeys _ =
    -- F1
    [ ("<F1>", spawn "/home/ross/.scripts/battery")
    -- F2
    , ("<F2>", spawn "/home/ross/.scripts/brightness down")
    -- F3
    , ("<F3>", spawn "/home/ross/.scripts/brightness up")
    -- F4
    , ("<F4> <F4>", spawn "/home/ross/.scripts/monitor auto")
    , ("<F4> m", spawn "/home/ross/.scripts/monitor mirror")
    , ("<F4> o", spawn "/home/ross/.scripts/monitor off")
    , ("<F4> <Tab>", spawn "/home/ross/.scripts/monitor off")
    -- F5
    -- Use this to do the keyboard stuff instead
    , ("<F5>", spawn "/home/ross/.scripts/keyboard setup notify")
    -- F6
    , ("<F6>", spawn "/home/ross/.scripts/volume toggle")
    , ("S-<F6>", spawn "/home/ross/.scripts/music-control play")
    -- F7
    , ("<F7>", spawn "/home/ross/.scripts/volume down")
    , ("S-<F7>", spawn "/home/ross/.scripts/music-control prev")
    -- F8
    , ("<F8>", spawn "/home/ross/.scripts/volume up")
    , ("S-<F8>", spawn "/home/ross/.scripts/music-control next")
    -- F9
    , ("<F9>", spawn "sudo /home/ross/.scripts/keyboard_backlight down")
    -- F10
    , ("<F10>", spawn "sudo /home/ross/.scripts/keyboard_backlight up")
    -- F11
    , ("<F11>", spawn "samsung-tools -c cycle && notify-send \"$(samsung-tools -c status)\"")
    -- F12
    , ("<F12>", spawn "samsung-tools -W toggle && notify-send -i /usr/share/icons/gnome/48x48/devices/network-wireless.png \"$(samsung-tools -W status)\"")
    ]
     -- , ("C-<F4>", spawn "/home/ross/.scripts/wallpaper random")
    -- , ("S-<F12>", spawn "sudo /home/ross/.scripts/wifi toggle && notify-send -i /usr/share/icons/gnome/scalable/apps/bluetooth-symbolic.svg \"$(samsung-tools -B status)\"")
    -- ]

mediaKeys :: XConfig Layout -> [(String, X ())]
mediaKeys _ =
    [ ("<XF86AudioPlay>", spawn "/home/ross/.scripts/music-control play")
    , ("<XF86AudioNext>", spawn "/home/ross/.scripts/music-control next")
    , ("<XF86AudioPrev>", spawn "/home/ross/.scripts/music-control prev")
    -- These are the functions keys on my laptop
    , ("<XF86AudioMute>", spawn "/home/ross/.scripts/volume toggle")
    , ("<XF86AudioLowerVolume>", spawn "/home/ross/.scripts/volume down")
    , ("<XF86AudioRaiseVolume>", spawn "/home/ross/.scripts/volume up")
    -- Shift version of the above to make up for lack of controls
    , ("S-<XF86AudioMute>", spawn "/home/ross/.scripts/music-control play")
    , ("S-<XF86AudioLowerVolume>", spawn "/home/ross/.scripts/music-control prev")
    , ("S-<XF86AudioRaiseVolume>", spawn "/home/ross/.scripts/music-control next")
    ]

focusKeys  :: XConfig Layout -> [(String, X ())]
focusKeys c =
    [
    -- moving focus
    ("M-<L>", windows W.focusUp)
    , ("M-<R>", windows W.focusDown)
    -- , ("M-<Space>", windows W.focusMaster)
    , ("M-<Backspace>", swapNextScreen >> nextScreen)
    , ("M-S-<Backspace>", nextScreen)
    , ("M-n", moveTo Next EmptyWS)
    , ("M-<Home>", moveTo Prev NonEmptyWS)
    , ("M-<End>", moveTo Next NonEmptyWS)
    , ("M-<Tab>", toggleWS' ["NSP"])
    -- , ("M-<Backspace>", windows $ showSpare)
    -- moving windows
    , ("M-C-<L>", windows W.swapUp)
    , ("M-C-<R>", windows W.swapDown)
    , ("M-C-<Space>", windows W.swapMaster)
    , ("M-C-<Backspace>", shiftNextScreen >> nextScreen)
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
