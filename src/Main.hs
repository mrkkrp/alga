-- -*- Mode: Haskell; -*-
--
-- Main module of ALGA describes logic of the program on top level.
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

module Main (main) where

import Control.Monad
import Options.Applicative
import System.Directory (getHomeDirectory, doesFileExist, getCurrentDirectory)
import System.FilePath
import qualified Data.Map as M
import qualified Data.Text.Format as F
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Alga.Configuration
import Alga.Interaction

data Opts = Opts
    { opInterac :: Bool
    , opSeed    :: Int
    , opBeats   :: Double
    , opTarget  :: String
    , opLicense :: Bool
    , opVersion :: Bool
    , opFiles   :: [String] }

main :: IO ()
main = execParser opts >>= f
    where f Opts { opLicense = True } = T.putStr license
          f Opts { opVersion = True } = F.print "ALGA {}\n" (F.Only version)
          f Opts { opFiles   = []   } = g $ interaction version
          f Opts { opInterac = True
                 , opFiles   = ns   } = g $ cmdLoad ns >> interaction version
          f Opts { opSeed    = s
                 , opBeats   = b
                 , opTarget  = trg
                 , opFiles   = ns   } = g $ cmdLoad ns >> cmdMake s b trg
          g x     = T.putStrLn notice >> runAlga x
          version = "0.1.0"

notice :: T.Text
notice =
    "ALGA Copyright © 2015 Mark Karpov\n\n\
    \This program comes with ABSOLUTELY NO WARRANTY. This is free software,\n\
    \and you are welcome to redistribute it under certain conditions; see\n\
    \GNU General Public License for details.\n"

license :: T.Text
license =
    "ALGA — algorithmic automation for various DAWs.\n\
    \Copyright © 2015 Mark Karpov\n\
    \\n\
    \ALGA is free software: you can redistribute it and/or modify it under the\n\
    \terms of the GNU General Public License as published by the Free Software\n\
    \Foundation, either version 3 of the License, or (at your option) any\n\
    \later version.\n\
    \\n\
    \ALGA is distributed in the hope that it will be useful, but WITHOUT ANY\n\
    \WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS\n\
    \FOR A PARTICULAR PURPOSE. See the GNU General Public License for more\n\
    \details.\n\
    \\n\
    \You should have received a copy of the GNU General Public License along\n\
    \with this program. If not, see <http://www.gnu.org/licenses/>.\n"

runAlga :: AlgaIO () -> IO ()
runAlga e = do
  params <- loadConfig
  wdir   <- getCurrentDirectory
  void $ runAlgaInt e
       AlgaSt { stPrevLen = lookupCfg params "prvlen" 18
              , stSrcFile = lookupCfg params "src"    wdir </> "foo.ga" }
       AlgaCfg { cfgPrecision = lookupCfg params "precision" 0.01
               , cfgPrompt    = lookupCfg params "prompt"    "> "
               , cfgVerbose   = lookupCfg params "verbose"   True }

loadConfig :: IO Params
loadConfig = do
  home <- getHomeDirectory
  let file = home </> ".alga"
  exist <- doesFileExist file
  if exist
  then do params <- parseConfig file <$> T.readFile file
          case params of
            Right x -> return x
            Left  _ -> return M.empty
  else return M.empty

opts :: ParserInfo Opts
opts =  info (helper <*> options)
      ( fullDesc
     <> progDesc "starts ALGA interpreter or patches given XML file"
     <> header "alga — algorithmic automation for various DAWs" )

options :: Parser Opts
options = Opts
  <$> switch
  ( long "interactive"
  <> short 'i'
  <> help "Start ALGA in interactive mode" )
  <*> option auto
  ( long "seed"
  <> short 's'
  <> metavar "SEED"
  <> value dfltSeed
  <> help ("Set seed for random numbers, default is " ++ show dfltSeed) )
  <*> option auto
  ( long "beats"
  <> short 'b'
  <> metavar "BEATS"
  <> value dfltBeats
  <> help ("Set total time in whole notes, default is " ++ show dfltBeats) )
  <*> strOption
  ( long "target"
  <> short 't'
  <> metavar "TARGET"
  <> value ""
  <> help "Specify target XML file for patching" )
  <*> switch
  ( long "license"
  <> help "Show license of the program" )
  <*> switch
  ( long "version"
  <> help "Show version of the program" )
  <*> many (strArgument $ metavar "FILES")
