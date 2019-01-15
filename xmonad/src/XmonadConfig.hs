{-# LANGUAGE UnicodeSyntax #-}

module XmonadConfig where

import XMonad
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageHelpers
import XMonad.StackSet as W

myModKey ∷ KeyMask
myModKey = mod4Mask

myTerminal, myLauncher, myFileBrowser, myWebBrowser ∷ String
myTerminal = "sakura"
myFileBrowser = "thunar"
-- myFileBrowser = "pantheon-files"
myWebBrowser = "firefox && notify-send -i /usr/share/icons/elementary/actions/48/process-stop.svg \"Firefox exited\""
myLauncher = "rofi -matching fuzzy -modi combi -show combi -combi-modi drun,run"

unfocusedColour, secondaryColour, highlightColour ∷ String
unfocusedColour = "#303030"
secondaryColour = "#707070"
highlightColour = "#57C7FF"

-- normBord = "#343C48"
-- focdBord = "#6986a0"
normBord = unfocusedColour
focdBord = highlightColour
fore     = "#DEE3E0"
back     = "#282c34"
winType = "#c678dd"

gapSize ∷ Int
gapSize = 4

myBorderWidth ∷ Dimension
myBorderWidth = 1

numberOfWorkspaces :: Int
numberOfWorkspaces = 6

myWorkspaces :: [WorkspaceId]
-- myWorkspaces = map lojShow [1 .. numberOfWorkspaces]
myWorkspaces = ["I","II","III","IV","V","VI"]


scratchpads ∷ [NamedScratchpad]
scratchpads = [
    NS "conky" "conky -c /home/ross/.config/conky/conky.conf" (className =? "conky") (doFullFloat)
    , NS "thunar" "thunar" (className =? "Thunar") (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
    , NS "calc" "gnome-calculator" (className =? "Gnome-calculator") (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
 ]

-- This appears to be more complicated than necessary because it is allowing for the conversion of numbers greater than 9. It breaks a number into its digits using toDigits, then maps the digit lookup to each digit and concats the result.
lojShow ∷ Int → String
lojShow n = concatMap (lojDigits!!) $ toDigits n

lojDigits ∷ [String]
lojDigits = ["no","pa","re","ci","vo","mu","xa","bi","so"]

toDigits ∷ Int → [Int]
toDigits n = let ds = length $ show n in reverse . take ds . unfold (`divMod` 10) $ n

unfold ∷ (a → (a,b)) → a → [b]
unfold f x = b : unfold f a
    where (a,b) = f x

getWorkspace ∷ Int → WorkspaceId
getWorkspace k
    | k < numberOfWorkspaces = myWorkspaces !! k
    | otherwise = head myWorkspaces