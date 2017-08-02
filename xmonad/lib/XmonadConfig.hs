{-# LANGUAGE UnicodeSyntax #-}

module XmonadConfig where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageHelpers

myTerminal ∷ String
myTerminal = "sakura"

unfocusedColour, secondaryColour, highlightColour ∷ String
unfocusedColour = "#303030"
secondaryColour = "#707070"
highlightColour = "#57C7FF"

gapSize ∷ Int
gapSize = 3

numberOfWorkspaces :: Int
numberOfWorkspaces = 6

myWorkspaces :: [WorkspaceId]
myWorkspaces = map lojShow [1 .. numberOfWorkspaces]

getWorkspace ∷ Int → WorkspaceId
getWorkspace k
    | k < numberOfWorkspaces = myWorkspaces !! k
    | otherwise = head myWorkspaces

scratchpads ∷ [NamedScratchpad]
scratchpads = [
    --  NS "clock" "urxvt -e /home/ross/.scripts/clock" (title =? "clock") (customFloating $ W.RationalRect (3/9) (4/9) (3/9) (1/9))
    NS "clock" "aclock" (className =? "Aclock") (customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3))
    -- , NS "conky" "conky -c /home/ross/.config/conky/conky.conf" (className =? "conky") (customFloating $ W.RationalRect (0) (0) (1) (1))
    , NS "conky" "conky -c /home/ross/.config/conky/conky.conf" (className =? "conky") (doFullFloat)
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
