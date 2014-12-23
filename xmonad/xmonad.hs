import XMonad hiding ((|||))

import System.Exit
import qualified System.IO.UTF8 as IO
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Maybe ( isJust )
--import Data.Ratio ((%))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
-- import XMonad.Actions.SinkAll
--import XMonad.Actions.FloatKeys
--import XMonad.Actions.NoBorders
import XMonad.Hooks.DynamicLog
-- provides ewmh hooks for making touchegg work
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
--import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
--import XMonad.Layout.PerWorkspace
--import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
--import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig
--import XMonad.Util.Loggers
--import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
--import XMonad.Util.Scratchpad

main :: IO ()
main = xmonad .
	ewmh $ --adds support of EWMH functions, makes touchegg work
	rossConfig

----------------------------------------------------------------------
-- config itself

rossConfig = defaultConfig {
	terminal           = "lxterminal",
	modMask            = mod4Mask,
	workspaces         = myWorkspaces,
	borderWidth        = 1,
	normalBorderColor  = darkGrey,
	focusedBorderColor = lightPurple,
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

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 5 :: Int]

------------------------------------------------------------------------
-- keybindings

myKeyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeyBindings c = mkKeymap c (rawKeys c)

rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = concatMap ($ c) keymaps where
	keymaps = [baseKeys, spawnKeys, fKeys, focusKeys]

baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys _ =
	[ ("M-h", spawn "hibernate")
	 , ("M-<Escape>", kill)
	 , ("M-C-r", spawn "/home/ross/.scripts/xmonad_recompile")
	 , ("M-C-S-r", restart "/home/ross/.xmonad/xmonad" True)
	 , ("M-b", sendMessage ToggleStruts)
	 , ("M-C-S-<Escape>", io exitSuccess)
	]

spawnKeys _ =
	 [ ("M-<Return>", spawn "urxvt")
	 , ("M-a", spawn "atom")
	 , ("M-c", spawn "gnome-calculator")
	 , ("M-e", spawn "nemo")
	 , ("M-f", spawn "firefox-aurora")
	 , ("M-r", spawn "firefox-aurora -P Rdio -no-remote")
	 , ("M-u", spawn "/home/ross/.scripts/keyboard_setup_udev")
	 , ("M-v", spawn "urxvt -e alsamixer")
	-- Toggle Keyboard mapping
	 , ("M-S-k", spawn "xmodmap /home/ross/.xmodmap_sbrackets && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Square Brackets Activated\"")
	 , ("M-k", spawn "xmodmap /home/ross/.xmodmap_braces && notify-send -i /usr/share/icons/gnome/48x48/devices/keyboard.png \"Braces Activated\"")
	]

fKeys _ =
	[("<F1>", spawn "sudo chvt 2")
	 , ("<F2>", spawn "/home/ross/.scripts/brightness down")
	 , ("<F3>", spawn "/home/ross/.scripts/brightness up")
	 , ("<F4>", spawn "/home/ross/.scripts/screens")
	 , ("S-<F6>", spawn "/home/ross/.scripts/volume headphone_switch")
	 , ("<F6>", spawn "/home/ross/.scripts/volume toggle")
	 , ("<F7>", spawn "/home/ross/.scripts/volume down")
	 , ("<F8>", spawn "/home/ross/.scripts/volume up")
	 , ("<F9>", spawn "sudo /home/ross/.scripts/keyboard_backlight down")
	 , ("<F10>", spawn "sudo /home/ross/.scripts/keyboard_backlight up")
	 , ("<F11>", spawn "samsung-tools -c cycle && notify-send \"$(samsung-tools -c status)\"")
	 , ("<F12>", spawn "samsung-tools -W toggle && notify-send -i /usr/share/icons/gnome/48x48/devices/network-wireless.png \"$(samsung-tools -W status)\"")
	]

focusKeys c =
	[
	-- moving focus
	("M-<U>", windows W.focusUp)
	, ("M-<D>", windows W.focusDown)
	, ("M-<L>", windows W.focusMaster)
	, ("M-<Home>", moveTo Prev NonEmptyWS)
	, ("M-<End>", moveTo Next NonEmptyWS)
	, ("M-<Space>", swapNextScreen)
	, ("M-S-<Space>", nextScreen)
	, ("M-<Tab>", toggleWS)
	, ("M-n",  moveTo Next EmptyWS)
	-- moving windows
	, ("M-C-<U>", windows W.swapUp)
	, ("M-C-<D>", windows W.swapDown)
	, ("M-C-<L>", windows W.swapMaster)
	, ("M-C-<Home>", shiftTo Prev NonEmptyWS >> moveTo Prev NonEmptyWS)
	, ("M-C-<End>", shiftTo Next NonEmptyWS >> moveTo Next NonEmptyWS)
	, ("M-C-<Space>", shiftNextScreen)
	, ("M-C-n",  shiftTo Next EmptyWS)
	-- fullscreen and floating
	, ("M-S-<R>", withFocused $ windows . (\w -> W.float w (W.RationalRect (0.0) (0.0) (1.0) (1.0))))
	, ("M-<R>", withFocused $ windows . W.sink)
	-- change layout
	, ("M-,", sendMessage Shrink)
	, ("M-.", sendMessage Expand)
	, ("M-~", sendMessage NextLayout)
	, ("M-<", sendMessage (IncMasterN (-1)))
	, ("M->", sendMessage (IncMasterN 1))
	]
	++
	[("M-" ++ show k, windows $ W.greedyView i) | (k, i) <- zip [1..5::Int] (XMonad.workspaces c)]
	++
	[("M-C-" ++ show k, windows $ W.greedyView i . W.shift i) | (k, i) <- zip [1..5 :: Int] (XMonad.workspaces c)]
	++
	[("M-S-C-" ++ show k, windows $ W.shift i) | (k, i) <- zip [1..5::Int] (XMonad.workspaces c)]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

	-- mod-button1, Set the window to floating mode and move by dragging
	[ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))

	-- mod-button2, Raise the window to the top of the stack
	, ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

	-- mod-button3, Set the window to floating mode and resize by dragging
	, ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))

	-- you may also bind events to the mouse scroll wheel (button4 and button5)
	]
------------------------------------------------------------------------
-- window rules

myManageHook = composeAll . concat $
		[
		[ isFullscreen --> doFullFloat ]
		]

------------------------------------------------------------------------
-- status bar and logging

lightGrey = "#707070"
darkGrey = "#303030"
lightPurple = "#A091BD"

myLogHook = myLogger 0 >> myLogger 1

myLogger :: Int -> X ()
myLogger scr = makeLogString scr >>= io . dumpToPipe scr

statusPipePath scr
	| scr == 0 = "/tmp/xmonad_status_pipe1"
	| scr == 1 = "/tmp/xmonad_status_pipe2"

dumpToPipe :: Int -> String -> IO ()
dumpToPipe scr s = do
	IO.writeFile (statusPipePath scr) $ UTF8.decodeString (s++"\n")

makeLogString :: Int -> X String
makeLogString scr = do
    st <- gets windowset
    sort' <- getSortByIndex

    -- workspace list
    let ws = W.hidden st ++ map W.workspace (W.current st : W.visible st)
    return $ UTF8.encodeString . concatMap (makeDot st scr) . sort' $ ws

--makeDot :: WindowSet -> Int -> WorkspaceId -> String
makeDot st scr w
	| isCurrentOnThisScreen && isCurrent		= xmobarColor lightPurple "" "●"
	| not isCurrentOnThisScreen && isCurrent	= xmobarColor lightGrey "" "●"
	| not isCurrentOnThisScreen && isVisible	= xmobarColor lightPurple "" "●"
	| isCurrentOnThisScreen && isVisible		= xmobarColor lightGrey "" "●"
	| isJust (W.stack w)	= xmobarColor darkGrey "" "●"
	| otherwise				= xmobarColor darkGrey "" "○"
	where
		isCurrentOnThisScreen = W.screen (W.current st) == S scr
		isCurrent = W.tag w == W.currentTag st
		isVisible = W.tag w `elem` visibles
		visibles = map (W.tag . W.workspace) (W.visible st)


------------------------------------------------------------------------
-- layouts

myLayouts = smartBorders $
		avoidStruts $
		--showName $
		spacing 5 $
		tiled ||| Mirror tiled ||| Full
	where
		tiled = Tall 1 (3 / 100) (1 / 2)
