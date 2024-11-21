{-# LANGUAGE FlexibleInstances #-}

-- | Extensible state for XMonad

module XMonad.Ext where

import XMonad.Core
import XMonad.Operations
import XMonad.StackSet as W
import Graphics.X11.Xlib
import qualified XMonad.ExtensionUtils as XS

import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty((:|)))

newtype BuriedWindows = BuriedWindows (M.Map WorkspaceId [Window])
  deriving (Show, Read)

instance ExtensionClass BuriedWindows where
  initialValue = BuriedWindows $ M.empty
  extensionType = PersistentExtension

bury :: StackSet i l a s sd -> StackSet i l a s sd
bury s = s { current = (current s)
              { workspace = (workspace (current s))
                { stack = bury' (stack (workspace (current s))) }}}
  where
    bury' Nothing = Nothing
    bury' (Just (Stack _ (l:ls) rs))  = Just $ Stack l ls rs
    bury' (Just (Stack _ [] (r:rs)))  = Just $ Stack x xs []
        where (x :| xs) = NE.reverse (r :| rs)
    bury' (Just (Stack _ [] []))     = Nothing

buryX :: X()
buryX = do
  Workspace { tag = t, stack = Just (Stack focused _ _) } <- gets $ workspace . current . windowset
  windows bury
  BuriedWindows m <- XS.get
  let currentBuriedWindows = M.findWithDefault [] t m
  let newBuriedWindows = BuriedWindows $ M.insert t (focused:currentBuriedWindows) m
  XS.put newBuriedWindows


-- | unbury the last buried window
unbury :: Maybe a -> StackSet i l a s sd -> StackSet i l a s sd
unbury lastBuriedWin s = s { current = (current s)
                              { workspace = (workspace (current s))
                                { stack = unbury' lastBuriedWin . stack . workspace . current $ s}}}

unbury' :: Maybe a -> Maybe (Stack a) -> Maybe (Stack a)
unbury' (Just x) (Just (Stack t ls rs)) = Just $ Stack t ls (reverse (x:rs))
unbury' (Just x) Nothing                = Just $ Stack x [] []
unbury' Nothing  s                      = s

unburyX :: X()
unburyX = do
  Workspace { tag = t } <- gets $ workspace . current . windowset
  BuriedWindows m <- XS.get
  let currentBuriedWindows = M.findWithDefault [] t m
  let lastBuriedWin = listToMaybe currentBuriedWindows
  let newBuriedWindows = BuriedWindows $ M.insert t (drop 1 currentBuriedWindows) m
  windows $ unbury lastBuriedWin
  XS.put newBuriedWindows
