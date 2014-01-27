{-# LANGUAGE DeriveDataTypeable #-}

import XMonad hiding ((|||))
 
import System.Exit
--import System.IO(openFile,hSetEncoding,hClose,IOMode(..))
import qualified System.IO.UTF8 as IO
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Ratio ((%))
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- to force regular status updates
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Timer
import Data.Monoid (All(..))

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.NoBorders
import XMonad.Hooks.DynamicLog
-- provides ewmh hooks for making touchegg work
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
--import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
--import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
--import XMonad.Util.Scratchpad

main = do 
	xmonad $
		ewmh $ --adds support of EWMH functions, makes touchegg work
		--withUrgencyHook NoUrgencyHook $
		rossConfig

----------------------------------------------------------------------
-- config itself

rossConfig = defaultConfig {
	terminal           = "lxterminal",
	modMask            = mod4Mask,
	workspaces         = myWorkspaces,
	borderWidth        = 1,
	normalBorderColor  = "#454545",
	focusedBorderColor = "#f9b857",
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
 
--myWorkspaces = ["term","web","misc"]
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 5 :: Int] 

------------------------------------------------------------------------
-- keybindings
 
myKeyBindings = \c -> mkKeymap c $
	 -- other stuff
	 --[ ("M-t", spawn "lxterminal")
	 --, ("M-<Return>", spawn "lxterminal")
	 [ ("M-<Return>", spawn "lxterminal")
	 , ("M-r", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
	 , ("M-S-<Escape>", io (exitWith ExitSuccess))  
	 , ("M-q", kill)
	 , ("M-b", sendMessage ToggleStruts)
	 -- moving focus
	 , ("M-<L>", windows W.focusUp)
	 , ("M-<R>", windows W.focusDown)
	 , ("M-<Space>", windows W.focusMaster) 
	 , ("M-<Home>", moveTo Prev NonEmptyWS)
	 , ("M-<End>", moveTo Next NonEmptyWS)
	 , ("M-x", swapNextScreen)
	 , ("M-S-x", nextScreen)
	 , ("M-<Tab>", toggleWS)
	 , ("M-n",  moveTo Next EmptyWS)
	 , ("M-C-n",  shiftTo Next EmptyWS >> moveTo Next EmptyWS)
	 -- moving windows
	 , ("M-C-<L>", windows W.swapUp)
	 , ("M-C-<R>", windows W.swapDown)
	 , ("M-C-<Space>", windows W.swapMaster) 
	 , ("M-C-<Home>", shiftTo Prev NonEmptyWS >> moveTo Prev NonEmptyWS)
	 , ("M-C-<End>", shiftTo Next NonEmptyWS >> moveTo Next NonEmptyWS)
	 , ("M-C-x", shiftNextScreen)
	 --, ("M-<D>", withFocused $ windows . W.sink) 
	 -- change layout
	 , ("M-<D>", sendMessage Shrink)
	 , ("M-<U>", sendMessage Expand)
	 , ("M-~", sendMessage NextLayout)
	 --, ("M-,", sendMessage (IncMasterN 1))
	 --, ("M-.", sendMessage (IncMasterN (-1)))
	 ]
	 ++
	 [("M-" ++ show k, windows $ W.greedyView i) | (k, i) <- zip [1..5] (XMonad.workspaces c)]
	 ++
	 [("M-C-" ++ show k, windows $ W.greedyView i . W.shift i) | (k, i) <- zip [1..5] (XMonad.workspaces c)]
	 ++
	 [("M-S-C-" ++ show k, windows $ W.shift i) | (k, i) <- zip [1..5] (XMonad.workspaces c)]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
	-- mod-button1, Set the window to floating mode and move by dragging
	[ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
									   >> windows W.shiftMaster))
 
	-- mod-button2, Raise the window to the top of the stack
	, ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
	-- mod-button3, Set the window to floating mode and resize by dragging
	, ((modm, button3), (\w -> focus w >> mouseResizeWindow w
									   >> windows W.shiftMaster))
 
	-- you may also bind events to the mouse scroll wheel (button4 and button5)
	]
------------------------------------------------------------------------
-- window rules
 
-- myManageHook = scratchpadManageHookDefault <+> (composeAll . concat $
myManageHook = composeAll . concat $
		[ [ className =? c --> doFloat | c <- floats ]
		, [ resource  =? "desktop_window" --> doIgnore ]
		-- , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
		, [ isFullscreen --> doFullFloat ]
		]
	where 
		floats = ["MPlayer", ".", "feh"]
		--moveTo = doF . W.shift
 
------------------------------------------------------------------------
-- status bar and logging

lightGrey = "#909090"
darkGrey = "#303030"

myLogHook = dynamicLogWithPP $ defaultPP

	-- display current workspace as darkgrey on light grey (opposite of 
	-- default colors)
	{ ppCurrent         = const$ dzenColor lightGrey "" "●"

	-- display other workspaces which contain windows as a brighter grey
	, ppHidden          = const$ dzenColor darkGrey "" "●"
	, ppHiddenNoWindows	= const$ dzenColor darkGrey "" "○"

	-- put the displays on screen first in the list, and in square brackets
	--, ppVisible			= wrap "[" "]"
	, ppSort 			= getSortByIndex
	--, ppSort 			= getSortByXineramaRule

	-- display other workspaces with no windows as a normal grey
	--, ppHiddenNoWindows = dzenColor "#606060" "" . pad 

	-- display the current layout as a brighter grey
	--, ppLayout          = dzenColor "#909090" "" . pad 
	, ppLayout          = const ""

	-- shorten if it goes over 100 characters
	, ppTitle           = shorten 100  

	-- no separator between workspaces
	, ppWsSep           = ""

	-- put a few spaces between each object
	, ppSep             = " "

	-- put the time in
	--, ppExtras			= [ colouredTime ]

	--rearrage time to be fore the window title
	--, ppOrder = \(ws:lay:tit:time:rest) -> (ws:lay:time:tit:rest)

	-- output to the handle we were given as an argument
	--, ppOutput          = hPutStrLn statusPipe
	, ppOutput         = dumpToFile
	}

statusFile = "/home/ross/.xmonad_status"

dumpToFile :: String -> IO ()
dumpToFile s = IO.appendFile statusFile $ UTF8.decodeString (s++"\n")
 
------------------------------------------------------------------------
-- layouts
 
myLayouts =	smartBorders $
				avoidStruts $
				showWName $
				spacing 2 $
				--toggleLayouts (noBorders Full) $
				tiled ||| Mirror tiled ||| (noBorders Full)
	where
		tiled = Tall 1 (3 / 100) (1 / 2)
