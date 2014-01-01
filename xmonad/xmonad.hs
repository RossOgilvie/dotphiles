{-# LANGUAGE DeriveDataTypeable #-}

import XMonad hiding ((|||))
 
import System.Exit
import System.IO
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
	--workspaces         = myWorkspaces,
	borderWidth        = 1,
	normalBorderColor  = "#454545",
	focusedBorderColor = "#f9b857",
	focusFollowsMouse  = False,
	manageHook         = myManageHook <+> manageDocks,
	handleEventHook    = clockEventHook,
	startupHook        = clockStartupHook,
	logHook            = myLogHook,
	layoutHook         = myLayouts,
	keys               = myKeyBindings,
	XMonad.clickJustFocuses = False
}

------------------------------------------------------------------------
-- workspaces
 
--myWorkspaces = ["term","web","misc"]
 
------------------------------------------------------------------------
-- keybindings
 
myKeyBindings = \c -> mkKeymap c $
	 -- other stuff
	 [ ("M-t", spawn "lxterminal")
	 , ("M-<Return>", spawn "lxterminal")
	 , ("M-r", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
	 , ("M-S-<Escape>", io (exitWith ExitSuccess))  
	 , ("M-q", kill)
	 -- moving focus
	 , ("M-<L>", windows W.focusUp)
	 , ("M-<R>", windows W.focusDown)
	 , ("M-x", nextScreen)
	 , ("M-<Tab>", toggleWS)
	 , ("M-<Space>", windows W.focusMaster) 
	 --, ("M-<Backspace>", focusUrgent)
	 , ("M-<Page_Up>", moveTo Prev NonEmptyWS)
	 , ("M-<Page_Down>", moveTo Next NonEmptyWS)
	 -- moving windows
	 , ("M-C-<L>", windows W.swapUp)
	 , ("M-C-<R>", windows W.swapDown)
	 , ("M-C-x", shiftNextScreen)
	 , ("M-C-<Space>", windows W.swapMaster) 
	 , ("M-<D>", withFocused $ windows . W.sink) 
	 -- change layout
	 , ("M-h", sendMessage Shrink)
	 , ("M-l", sendMessage Expand)
	 , ("M-~", sendMessage NextLayout)
	 , ("M-,", sendMessage (IncMasterN 1))
	 , ("M-.", sendMessage (IncMasterN (-1)))
	 ]
	 ++
	 [("M-" ++ show k, windows $ W.greedyView i) | (k, i) <- zip [1..9] (XMonad.workspaces c)]
	 ++
	 [("M-C-" ++ show k, windows $ W.greedyView i . W.shift i) | (k, i) <- zip [1..9] (XMonad.workspaces c)]
	 ++
	 [("M-S-C-" ++ show k, windows $ W.shift i) | (k, i) <- zip [1..9] (XMonad.workspaces c)]

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
 
myLogHook = dynamicLogWithPP $ defaultPP

	-- display current workspace as darkgrey on light grey (opposite of 
	-- default colors)
	{ ppCurrent         = dzenColor "#303030" "#909090" . pad 

	-- display other workspaces which contain windows as a brighter grey
	, ppHidden          = dzenColor "#909090" "" . pad 

	-- put the displays on screen first in the list, and in square brackets
	, ppVisible			= wrap "[" "]"
	, ppSort 			= getSortByXineramaRule

	-- display other workspaces with no windows as a normal grey
	--, ppHiddenNoWindows = dzenColor "#606060" "" . pad 

	-- display the current layout as a brighter grey
	, ppLayout          = dzenColor "#909090" "" . pad 

	-- if a window on a hidden workspace needs my attention, color it so
	, ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

	-- shorten if it goes over 100 characters
	, ppTitle           = shorten 100  

	-- no separator between workspaces
	, ppWsSep           = ""

	-- put a few spaces between each object
	, ppSep             = " -- "

	-- put the time in
	, ppExtras			= [ colouredTime ]

	--rearrage time to be fore the window title
	, ppOrder = \(ws:lay:tit:time:rest) -> (ws:lay:time:tit:rest)

	-- output to the handle we were given as an argument
	--, ppOutput          = hPutStrLn statusPipe
	, ppOutput         = dumpToFile
	}

statusFile = "/home/ross/.xmonad_status"

dumpToFile :: String -> IO ()
dumpToFile s = do
	h <- openFile statusFile AppendMode
	hPutStrLn h s
	hClose h

colouredTime :: Logger
colouredTime = wrapL "^fg(#60da11)" "^fg(#909090)" $ date "%a %b %d %H:%M"


-- wrapper for the Timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
  initialValue = TID 0

-- put this in your startupHook
-- start the initial timer, store its id
clockStartupHook = startTimer 29 >>= XS.put . TID

-- put this in your handleEventHook
clockEventHook e = do               -- e is the event we've hooked
  (TID t) <- XS.get                 -- get the recent Timer id
  handleTimer t e $ do              -- run the following if e matches the id
    startTimer 29 >>= XS.put . TID   -- restart the timer, store the new id
    ask >>= logHook.config          -- get the loghook and run it
    return Nothing                  -- return required type
  return $ All True                 -- return required type


--myPP statusPipe = xmobarPP {
--	ppOutput  = hPutStrLn statusPipe,
--	ppTitle   = xmobarColor "green"  "" . shorten 50,
--	ppUrgent  = xmobarColor "red" "" . wrap "!" "!"
--}
 
--myLogHook = dynamicLogWithPP . myPP
 
------------------------------------------------------------------------
-- layouts
 
myLayouts = avoidStruts $
				smartBorders $
				toggleLayouts (noBorders Full) $
				tiled ||| Mirror tiled ||| (noBorders Full)
	where
		tiled = Tall 1 (3 / 100) (1 / 2)
