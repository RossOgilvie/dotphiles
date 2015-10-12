{-# LANGUAGE UnicodeSyntax #-}

module XmonadConfig where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad


myTerminal = "urxvt"

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

getWorkspace k
    | k < numberOfWorkspaces = myWorkspaces !! k
    | otherwise = head myWorkspaces

scratchpads = [
     NS "clock" "urxvt -e /home/ross/.scripts/clock" (title =? "clock") (customFloating $ W.RationalRect (4/9) (4/9) (1/9) (1/9))
 ]

lojDigits = ["no","pa","re","ci","vo","mu","xa","bi","so"]
toDigits n = reverse . take ds . unfold (`divMod` 10) $ n
    where
        ds = length $ show n

lojShow n = concatMap (lojDigits!!) $ toDigits n

unfold ∷ (a → (a,b)) → a → [b]
unfold f x = b : unfold f a
    where (a,b) = f x
