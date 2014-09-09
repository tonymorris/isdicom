module System.FilePath.Dicom(
  isDicomFile
, dicomFileR
, dicomCodeFileR
, codeFileR
, FileR(..)
) where

import Control.Category(Category((.)))
import Control.Monad(Monad(return))
import Data.Bool(Bool)
import Data.Char(Char)
import Data.Eq(Eq((==)))
import Data.Int(Int)
import Data.Foldable(Foldable, any)
import Data.Functor(Functor(fmap))
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Prelude (($), Show)
import System.Directory(doesFileExist, doesDirectoryExist, getPermissions, readable)
import System.FilePath(FilePath)
import System.IO(IO, Handle, hClose, hReady, hGetChar, hSeek, openFile, SeekMode(AbsoluteSeek), IOMode(ReadMode))

data FileR =
  DoesNotExist
  | IsNotReadable
  | IsDirectory
  | IsNotDicom
  | IsDicom
  deriving (Eq, Show, Ord)

codeFileR ::
  FileR
  -> Int
codeFileR DoesNotExist =
  1
codeFileR IsNotReadable =
  2
codeFileR IsDirectory =
  3
codeFileR IsNotDicom =
  4
codeFileR IsDicom =
  0

dicomCodeFileR ::
  FilePath
  -> IO Int
dicomCodeFileR =
  fmap codeFileR . dicomFileR

dicomFileR ::
  FilePath
  -> IO FileR
dicomFileR p =
  do e <- doesFileExist p
     if e
       then
         do o <- getPermissions p
            if readable o
              then
                do b <- isDicomFile p
                   return $ if b
                              then
                                IsDicom
                              else
                                IsNotDicom
              else
                return IsNotReadable
       else
         do e' <- doesDirectoryExist p
            return $ if e'
                       then
                         IsDirectory
                       else
                         DoesNotExist

isDicomFile ::
  FilePath
  -> IO Bool
isDicomFile p =
  do h <- openFile p ReadMode
     hSeek h AbsoluteSeek 128
     d <- hChar4 h
     hClose h
     return (isDicom d)

isDicom ::
  Foldable f =>
  f (Char, Char, Char, Char)
  -> Bool
isDicom =
  any (\(c1, c2, c3, c4) -> [c1, c2, c3, c4] == "DICM")

hChar ::
  Handle
  -> IO (Maybe Char)
hChar h =
  do r <- hReady h
     if r
       then
         do c <- hGetChar h
            return (Just c)
       else
         return Nothing 

hChar4 ::
  Handle
  -> IO (Maybe (Char, Char, Char, Char))
hChar4 h =
  -- MaybeT
  let (.>>=.) :: Monad f => f (Maybe a) -> (a -> f (Maybe b)) -> f (Maybe b)
      i .>>=. f =
        do m <- i
           case m of
             Nothing -> return Nothing
             Just a -> f a
  in hChar h .>>=. \c1 ->
     hChar h .>>=. \c2 -> 
     hChar h .>>=. \c3 -> 
     hChar h .>>=. \c4 -> 
     return (Just (c1, c2, c3, c4))
