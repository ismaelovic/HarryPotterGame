{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskellAdventure (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskellAdventure"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A text-based adventure game set in Hogwarts"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/yourusername/haskellAdventure"
