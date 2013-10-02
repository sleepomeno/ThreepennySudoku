{-# LANGUAGE CPP#-}
module Paths (getStaticDir) where

import Control.Monad
import System.FilePath

#if CABAL
-- using cabal
import qualified Paths_sudoku (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = (</> "wwwroot") `liftM` Paths_sudoku.getDataDir

#else
using GHCi

getStaticDir :: IO FilePath
getStaticDir = return "../wwwroot/"

#endif
