import XMonad hiding ((|||))
 
import System.Exit
import qualified System.IO.UTF8 as IO
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Maybe ( isJust )
--import Data.Ratio ((%))
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
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
 
myKeyBindings c = mkKeymap c  $
	 -- apps
	 --[ ("M-<Return>", spawn "lxterminal")
	 [ ("M-<Return>", spawn "urxvt")
	 , ("M-e", spawn "nemo")
	 , ("M-v", spawn "pavucontrol")
	 , ("M-c", spawn "gnome-calculator")
	 , ("M-h", spawn "hibernate")
	 --other stuff
	 , ("M-r", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
	 , ("M-S-<Escape>", io exitSuccess)  
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
	 , ("M-m", withFocused $ windows . W.sink) 
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
