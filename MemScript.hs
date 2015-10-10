-- vim: sw=2: ts=2: expandtab: autoindent:
{-# LANGUAGE NoMonomorphismRestriction #-}
-- module MemScript where
-- module Main where

import System.IO
import Data.List
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe
import System.Environment (getArgs)

-- import System.Console.Readline
import System.Console.Haskeline
import System.Console.Haskeline.History


main = do filenames <- getArgs
          if length filenames == 1
            -- then memScript (head filenames)
            -- then initialize >> memScriptReadLine (head filenames)
            then runInputT defaultSettings $
                   memScriptReadLine (head filenames)
            else putStrLn "Usage: memscript <filename>"

memScriptReadLine filename =
   checkVersesByLine =<< liftIO(getVersesFromFile filename)

getVersesFromFile = liftM lines . getFileContents

getFileContents :: String -> IO String
getFileContents filename = hGetContents =<< openFile filename ReadMode

checkVersesByLine = checkVersesByLineWith (readlineUTF8withAddHistory "% ")

checkVersesByLineWith :: InputT IO (Maybe String) -> [String] -> InputT IO ()
checkVersesByLineWith _        []            = return ()
checkVersesByLineWith readLine verses@(v:vs) =
  -- do mg <- readLine
  --    case mg of Just g  -> checkOneLineAndContinue g
  --               Nothing -> handleEOF
  maybe handleEOF checkOneLineAndContinue =<< readLine
  where
  handleEOF = liftIO $ printDiff (concat $ intersperse "\n< " verses) ""
  checkOneLineAndContinue g
     | v == g    = checkVersesByLineWith readLine vs
     | otherwise = do liftIO (printDiff v g)
                      checkVersesByLineWith readLine verses


readlineUTF8withAddHistory prompt =
  do -- ms <- readline prompt
     -- whenJust addHistory ms
     ms <- getInputLine prompt
     return ms

-- whenJust = maybe (return ())

showDiff v g = concat $ intersperse "\n" ["===",
                                          "< "++v,
                                          "---",
                                          "> "++g,
                                          "==="]

printDiff v g = putStrLn $ showDiff v g
