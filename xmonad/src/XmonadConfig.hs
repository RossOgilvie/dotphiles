{-# LANGUAGE UnicodeSyntax #-}

module XmonadConfig where

import XMonad
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageHelpers

myModKey ∷ KeyMask
myModKey = mod4Mask

myTerminal ∷ String
myTerminal = "sakura"

unfocusedColour, secondaryColour, highlightColour ∷ String
unfocusedColour = "#303030"
secondaryColour = "#707070"
highlightColour = "#57C7FF"

gapSize ∷ Int
gapSize = 4

myBorderWidth ∷ Dimension
myBorderWidth = 1

numberOfWorkspaces :: Int
numberOfWorkspaces = 6

myWorkspaces :: [WorkspaceId]
myWorkspaces = map lojShow [1 .. numberOfWorkspaces]


scratchpads ∷ [NamedScratchpad]
scratchpads = [
    NS "conky" "conky -c /home/ross/.config/conky/conky.conf" (className =? "conky") (doFullFloat)
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