{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad(Monad((>>=)))
import System.FilePath.Dicom(dicomExitCodeFileR)
import System.Environment(getArgs)
import System.Exit(exitWith)
import System.IO(hPutStrLn, stderr, IO)

main ::
  IO ()
main = 
  do a <- getArgs
     case a of
       [] ->
         hPutStrLn stderr "isdicom <filepath>"
       (d:_) ->
         dicomExitCodeFileR d >>= exitWith
