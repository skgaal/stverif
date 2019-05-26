{- 
	This file is part of stverif.

    stverif is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    stverif is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with stverif.  If not, see <https://www.gnu.org/licenses/>.
  -}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.State
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory, doesFileExist)
import System.FilePath
import System.Posix.Files
import Data.List
import Data.Char (toLower)
import System.Environment ( getArgs )

data FileType = TYP | VAR | FUN | ST | UnknownFileType deriving (Ord, Eq, Show)

getFileType :: FilePath -> FileType
getFileType filename = getTypeHelper . map toLower . last $ wordsWhen (=='.') filename where
    getTypeHelper :: String -> FileType
    getTypeHelper "typ" = TYP
    getTypeHelper "var" = VAR
    getTypeHelper "fun" = FUN
    getTypeHelper "st"  = ST
    getTypeHelper _     = UnknownFileType
    wordsWhen     :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                          "" -> []
                          s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'


findLib :: FilePath -> IO [FilePath]
findLib filePath = do
    exists <- doesDirectoryExist filePath
    if not exists then return [] else do
      allFiles <- listDirectory filePath
      dirs  <- filterM (doesDirectoryExist . (filePath </>)) allFiles
      let prgFiles = filter (\s -> getFileType s /= UnknownFileType) allFiles
          files = (map (filePath </>) prgFiles) in
              if (null dirs)
                  then return files
                  else do ll <- mapM (\dir -> findLib (filePath </> dir)) dirs
                          return $ files ++ concat ll

findAll :: FilePath -> IO [FilePath]
findAll filePath = do
  allFiles <- listDirectory filePath
  dirs <- filterM (doesDirectoryExist . (filePath </>)) allFiles
  let prgFiles = filter (\s -> getFileType s /= UnknownFileType) allFiles
      files = (map (filePath </>) prgFiles) in
      if (null dirs)
      then return []
      else do ll <- mapM (\dir -> findAll (filePath </> dir)) (filter (\dir -> dir /= "Libraries" && dir /= "SourceLib" && dir /= "VCShared") dirs)
              return $ files ++ concat ll

nextInt :: MonadState Integer m => m Integer
nextInt = do
  n <- get
  put (n+1)
  return n

getLastFolderName :: FilePath -> String
getLastFolderName fpath = last $ wordsWhen (=='/') fpath
  where
    wordsWhen     :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                          "" -> []
                          s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

traverseDir :: (MonadIO m, MonadState Integer m) => [FilePath] -> FilePath -> m ()
traverseDir fileList filePath = do
    allFiles <- liftIO $ listDirectory filePath
    dirs <- filterM (liftIO . doesDirectoryExist . (filePath </>)) allFiles
    let prgFiles = filter (\s -> getFileType s /= UnknownFileType) allFiles
        files = (fileList ++ (map (filePath </>) prgFiles)) in
        if (null dirs) then
          if null prgFiles then
            return ()
          else do
            n <- nextInt
            liftIO . putStrLn $ getLastFolderName filePath ++ show n ++ " " ++ foldr (\a b -> a ++ " " ++ b) "" files
        else forM_ (filter (\dir -> dir /= "Libraries" && dir /= "SourceLib" && dir /= "VCShared") dirs) (\dir -> traverseDir fileList (filePath </> dir))

main :: IO ()
main = do
    args <- getArgs
    let root = head args in do
      libs <- findLib (root </> "Libraries")
      srclibs <- findLib (root </> "SourceLib")
      files <- findAll root
      evalStateT (traverseDir (libs ++ srclibs ++ files) root) 0
