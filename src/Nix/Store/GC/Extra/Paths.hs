{-# LANGUAGE TemplateHaskell #-}

module Nix.Store.GC.Extra.Paths (
  nixStore,
) where

import Data.Char (isSpace)
import Data.List qualified as List
import Language.Haskell.TH qualified as TH
import System.Process qualified as Process

-- | Location of the @nix-store@ executable
nixStore :: FilePath
nixStore =
  $( do
      path <- TH.runIO do
        path <- Process.readProcess "which" ["nix-store"] ""
        pure $ List.dropWhile isSpace $ List.dropWhileEnd isSpace path

      TH.stringE path
   )
