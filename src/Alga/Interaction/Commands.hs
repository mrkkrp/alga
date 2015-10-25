-- -*- Mode: Haskell; -*-
--
-- This module describes all supported ALGA commands. It also provides all
-- the functionality to load source files and patch XML files.
--
-- Copyright © 2015 Mark Karpov
--
-- ALGA is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- ALGA is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}

module Alga.Interaction.Commands
  ( processCmd
  , completionFunc
  , cmdBackend
  , cmdLoad
  , cmdMake
  , cmdPrefix )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Foldable (find)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ratio (approxRational)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , getHomeDirectory
  , setCurrentDirectory )
import System.Exit (exitSuccess)
import System.FilePath
  ( addTrailingPathSeparator
  , joinPath
  , replaceExtension
  , splitDirectories
  , takeFileName
  , (</>) )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Formatting
import qualified System.Console.Haskeline as L

import Alga.Interaction.Base
import Alga.Language
import Alga.Representation
import Alga.Translation

data Cmd = Cmd
  { cmdName :: String
  , cmdFunc :: String -> AlgaIO ()
  , cmdDesc :: T.Text
  , cmdComp :: CompletionScheme }

data Scale = Lin | Log deriving (Eq, Show)

data CompletionScheme = None | Files | Names deriving (Eq, Show)

commands :: [Cmd]
commands =
  [ Cmd "backend" cmdBackend "Set backend (name of DAW)"             None
  , Cmd "cd"      cmdCd      "Change working directory"              Files
  , Cmd "clear"   cmdClear   "Restore default state of environment"  None
  , Cmd "def"     cmdDef     "Print definition of given symbol"      Names
  , Cmd "help"    cmdHelp    "Show this help text"                   None
  , Cmd "lin"     cmdLin     "Linear scale conversion to ratio"      None
  , Cmd "load"    cmdLoad'   "Load definitions from given file"      Files
  , Cmd "log"     cmdLog     "Logarithmic scale conversion to ratio" None
  , Cmd "make"    cmdMake'   "Patch an XML file"                     Files
  , Cmd "prvlen"  cmdLength  "Set length of displayed results"       None
  , Cmd "purge"   cmdPurge   "Remove redundant definitions"          None
  , Cmd "pwd"     cmdPwd     "Print working directory"               None
  , Cmd "quit"    cmdQuit    "Quit the interactive environment"      None
  , Cmd "ratio"   cmdRatio   "Real number to ratio converter"        None
  , Cmd "save"    cmdSave    "Save current environment in file"      Files
  , Cmd "udef"    cmdUdef    "Remove definition of given symbol"     Names
  , Cmd "vol"     cmdVol     "Convert decibels to ratio"             None ]

processCmd :: T.Text -> AlgaIO ()
processCmd txt =
  case find g commands of
    Just Cmd { cmdFunc = f } -> f . T.unpack . T.strip $ args
    Nothing -> liftIO $
               fprint ("Unknown command, try " % string % "help.\n") cmdPrefix
  where g Cmd { cmdName = c } = c == dropCmdPrefix (T.unpack cmd)
        (cmd, args)           = T.break isSpace (T.strip txt)

completionFunc :: L.CompletionFunc AlgaIO
completionFunc = L.completeWordWithPrev Nothing " " getCompletions

getCompletions :: String -> String -> AlgaIO [L.Completion]
getCompletions prev word = do
  names <- liftEnv getRefs
  files <- L.listFiles word
  let cmds    = (cmdPrefix ++) . cmdName <$> commands
      g None  = []
      g Files = files
      g Names = f names
  return $ case words . reverse $ prev of
             []    -> f $ cmds ++ names
             (c:_) -> case c `elemIndex` cmds of
                        Just i  -> g . cmdComp $ commands !! i
                        Nothing -> f names
    where f = fmap L.simpleCompletion . filter (word `isPrefixOf`)

cmdBackend :: String -> AlgaIO ()
cmdBackend = setBackend . toBackend

cmdCd :: String -> AlgaIO ()
cmdCd path = liftIO $ do
  new     <- addTrailingPathSeparator . (</> path) <$> getCurrentDirectory
  present <- doesDirectoryExist new
  if present
  then do corrected <- canonicalizePath new
          setCurrentDirectory corrected
          fprint ("Changed to \"" % string % "\".\n") corrected
  else fprint ("Cannot cd to \"" % string % "\".\n") new

cmdClear :: String -> AlgaIO ()
cmdClear _ = liftEnv clearDefs >> liftIO (T.putStrLn "Environment cleared.")

cmdDef :: String -> AlgaIO ()
cmdDef arg = mapM_ f (words arg)
  where f name = liftEnv (getSrc name) >>= liftIO . T.putStr

cmdHelp :: String -> AlgaIO ()
cmdHelp _ = liftIO (T.putStrLn "Available commands:") >> mapM_ f commands
  where f Cmd { cmdName = c, cmdDesc = d } = liftIO $ fprint fmt cmdPrefix c d
        fmt = ("  " % string % (right 24 ' ' %. string) % text % "\n")

cmdLin :: String -> AlgaIO ()
cmdLin str = pRatio Lin (parseNum a 0) (parseNum b 1) (parseNum s 0)
  where (a:b:s:_) = parseArgs str

cmdLoad' :: String -> AlgaIO ()
cmdLoad' = cmdLoad . words

cmdLoad :: [String] -> AlgaIO()
cmdLoad = mapM_ loadOne

loadOne :: String -> AlgaIO ()
loadOne given = do
  file <- output given ""
  b    <- liftIO $ doesFileExist file
  if b
  then do contents <- liftIO $ T.readFile file
          case parseAlga (takeFileName file) contents of
            Right x -> do mapM_ f x
                          setFileName file
                          liftIO $ fprint
                                 ("\"" % string % "\" loaded successfully.\n")
                                 file
            Left  x -> liftIO $ fprint ("Parse error in " % string % ".\n") x
  else liftIO $ fprint ("Could not find \"" % string % "\".\n") file
    where f (Definition n t) = processDef n t
          f (Exposition   _) = return ()

cmdLog :: String -> AlgaIO ()
cmdLog str = pRatio Log (parseNum a 0) (parseNum b e) (parseNum s 0)
  where (a:b:s:_) = parseArgs str
        e         = exp 1

cmdMake' :: String -> AlgaIO ()
cmdMake' str =
  let (s:b:f:_) = parseArgs str
  in cmdMake (parseNum s dfltSeed)
             (parseNum b dfltBeats)
             f

cmdMake :: Int -> Double -> String -> AlgaIO ()
cmdMake s b f = do
  file    <- output f "xml"
  backend <- getBackend
  status  <- liftEnv $ patchAuto s b file backend
  let msg = if status == 0
            then "File patched successfully \"" % string % "\".\n"
            else "Failed to patch file \"" % string % "\".\n"
  liftIO $ fprint msg file

cmdLength :: String -> AlgaIO ()
cmdLength x = getPrevLen >>= setPrevLen . parseNum x

cmdPurge :: String -> AlgaIO ()
cmdPurge _ = do
  defs <- liftEnv topDefs
  liftEnv $ purgeEnv defs
  liftIO $ T.putStrLn "Environment purged."

cmdPwd :: String -> AlgaIO ()
cmdPwd _ = liftIO (getCurrentDirectory >>= putStrLn)

cmdQuit :: String -> AlgaIO ()
cmdQuit _ = liftIO exitSuccess

cmdRatio :: String -> AlgaIO ()
cmdRatio = pRatio Lin 0 1 . (`parseNum` 0)

cmdSave :: String -> AlgaIO ()
cmdSave given = do
  file   <- output given ""
  src    <- liftEnv fullSrc
  result <- liftIO (try (T.writeFile file src) :: IO (Either SomeException ()))
  let fmt = "Environment saved as \"" % string % "\".\n"
  case result of
    Right _ -> do setFileName file
                  liftIO $ fprint fmt file
    Left  e -> spitExc e

cmdUdef :: String -> AlgaIO ()
cmdUdef arg = mapM_ f (words arg)
  where f name = do
          liftEnv (remDef name)
          liftIO $ fprint ("Definition for «" % string % "» removed.\n") name

cmdVol :: String -> AlgaIO ()
cmdVol = pRatio Lin 0 1 . dBToRatio . (`parseNum` 0)
  where dBToRatio dB = 10 ** (dB / 10)

pRatio :: Scale -> Double -> Double -> Double -> AlgaIO ()
pRatio scale a b s = do
  ε <- getPrecision
  let σ = case scale of
            Lin -> (s - a) / (b - a)
            Log -> (log s - log a) / (log b - log a)
      x = approxRational σ ε
  liftIO $ fprint ("≈ " % string % "\n") (pRational x)

parseArgs :: String -> [String]
parseArgs str = words str ++ repeat ""

parseNum :: (Num a, Read a) => String -> a -> a
parseNum s x = fromMaybe x $ fst <$> listToMaybe (reads s)

output :: String -> String -> AlgaIO String
output given ext = do
  actual <- getSrcFile
  home   <- liftIO getHomeDirectory
  let a = if null ext then actual else replaceExtension actual ext
      g = joinPath . fmap f . splitDirectories $ given
      f x = if x == "~" then home else x
  return $ if null given then a else g

setFileName :: FilePath -> AlgaIO ()
setFileName path = (</> path) <$> liftIO getCurrentDirectory >>= setSrcFile

dropCmdPrefix :: String -> String
dropCmdPrefix arg
  | cmdPrefix `isPrefixOf` arg = drop (length cmdPrefix) arg
  | otherwise = arg

cmdPrefix :: String
cmdPrefix = ":"

spitExc :: SomeException -> AlgaIO ()
spitExc = liftIO . fprint ("× " % string % ".\n") . show
