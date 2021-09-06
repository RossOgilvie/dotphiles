module XmonadConfig where

import qualified Data.List as L
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

myModKey :: KeyMask
myModKey = mod4Mask

myTerminal, myLauncher, myFileBrowser, myWebBrowser :: String
myTerminal = "sakura"
myFileBrowser = "nemo"
-- myFileBrowser = "pantheon-files"
myWebBrowser = "firefox && notify-send -i /usr/share/icons/elementary/actions/48/process-stop.svg \"Firefox exited\""
myLauncher = "/home/ross/.config/rofi/launcher.sh"

myWindowSwitcher = "/home/ross/.config/rofi/combi-switcher.sh"

unfocusedColour, secondaryColour, highlightColour :: String
unfocusedColour = "#303030"
secondaryColour = "#707070"
highlightColour = "#57C7FF"

-- normBord = "#343C48"
-- focdBord = "#6986a0"
normBord = unfocusedColour

focdBord = highlightColour

fore = "#DEE3E0"

back = "#282c34"

winType = "#c678dd"

gapSize :: Integer
gapSize = 4

myBorderWidth :: Dimension
myBorderWidth = 1

numberOfWorkspaces :: Int
numberOfWorkspaces = 5

myWorkspaces :: [WorkspaceId]
-- myWorkspaces = map lojShow [1 .. numberOfWorkspaces]
--myWorkspaces = ["eins", "zwei", "drei", "vier", "fünf", "sechs"]
myWorkspaces = map ((++ "d") . show) [1 .. numberOfWorkspaces]

-- RationalRect are left margin, top margin, width, height in rational fraction of screen size
scratchpads :: [NamedScratchpad]
scratchpads =
    [ -- NS "eww" "eww open-many clock sliders windows powermenu" (className =? "Eww") defaultFloating
      -- NS "conky" "conky -c /home/ross/.config/conky/conky.conf" (className =? "conky") (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
      -- , NS "thunar" "thunar" (className =? "Thunar") (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
      NS "nemo" "nemo" (className =? "Nemo") (customFloating $ W.RationalRect (1 / 8) (1 / 8) (6 / 8) (6 / 8))
    , NS "terminal" "sakura" (className =? "Sakura") (customFloating $ W.RationalRect (1 / 2) 0 (1 / 2) 1)
    , NS "calc" "qalculate-gtk" (className =? "Qalculate-gtk") (customFloating $ W.RationalRect (4 / 6) (1 / 6) (2 / 6) (4 / 6))
    ]

-- This appears to be more complicated than necessary because it is allowing for the conversion of numbers greater than 9. It breaks a number into its digits using toDigits, then maps the digit lookup to each digit and concats the result.
-- lojShow ∷ Int → String
-- lojShow n = concatMap (lojDigits!!) $ toDigits n

-- lojDigits ∷ [String]
-- lojDigits = ["no","pa","re","ci","vo","mu","xa","bi","so"]

-- toDigits ∷ Int → [Int]
-- toDigits n = let ds = length $ show n in reverse . take ds . unfold (`divMod` 10) $ n

-- unfold ∷ (a → (a,b)) → a → [b]
-- unfold f x = b : unfold f a
--     where (a,b) = f x

getWorkspace :: Int -> WorkspaceId
getWorkspace k
    | k < numberOfWorkspaces = myWorkspaces !! k
    | otherwise = head myWorkspaces
