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

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Alga.Configuration
import Alga.Interaction
import Alga.Translation (toBackend)
import Control.Monad
import Data.Text.Lazy (Text)
import Data.Version (showVersion)
import Formatting
import Numeric.Natural
import Options.Applicative
import Path
import Path.IO
import Paths_alga (version)
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

-- | ALGA application command line options.

data Opts = Opts
  { opInterac :: Bool  -- ^ Do we run in interactive mode?
  , opBackend :: String -- ^ Which backend to use?
  , opSeed    :: Natural -- ^ Seed for random number generator
  , opBeats   :: Double -- ^ Duration as number of whole notes
  , opTarget  :: String -- ^ XML file for patching
  , opLicense :: Bool  -- ^ Whether to show license
  , opVersion :: Bool  -- ^ Whether to show program's version
  , opFiles   :: [FilePath] -- ^ Source files to load
  }

-- | Entry point for the whole thing.

main :: IO ()
main = execParser opts >>= f
  where f Opts { opLicense = True } = T.putStr license
        f Opts { opVersion = True } = fprint ("ALGA " % string % "\n") ver
        f Opts { opFiles   = []
               , opBackend = β    } = g β interaction
        f Opts { opInterac = True
               , opBackend = β
               , opFiles   = ns   } = g β $ cmdLoad ns >> interaction
        f Opts { opBackend = β
               , opSeed    = s
               , opBeats   = b
               , opTarget  = trg
               , opFiles   = ns   } = g β $ cmdLoad ns >> cmdMake s b trg
        g β x   = T.putStrLn notice >> runAlga' (ξ β >> x)
        ξ β = unless (null β) (cmdBackend β)
        ver = showVersion version

-- | Shortish copyright notice.

notice :: Text
notice =
  "ALGA Copyright © 2015 Mark Karpov\n\n\
  \This program comes with ABSOLUTELY NO WARRANTY. This is free software,\n\
  \and you are welcome to redistribute it under certain conditions; see\n\
  \GNU General Public License for details.\n"

-- | Longer copyright notice.

license :: Text
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

-- | Read configuration file if present and run ALGA monad.

runAlga' :: Alga () -> IO ()
runAlga' e = do
  params <- loadConfig
  wdir   <- getCurrentDir
  dfname <- parseRelFile "foo.da"
  let dfltSrcFile = fromAbsFile (wdir </> dfname)
  srcFile <- parseAbsFile (lookupCfg params "src" dfltSrcFile)
  void $ runAlga e
    AlgaSt { stBackend = toBackend $ lookupCfg params "backend" "default"
           , stPrevLen = lookupCfg params "prvlen" 18
           , stSrcFile = srcFile }
    AlgaCfg { cfgPrecision = lookupCfg params "precision" 0.01
            , cfgPrompt    = lookupCfg params "prompt"    "> "
            , cfgVerbose   = lookupCfg params "verbose"   True }

-- | Read configuration file.

loadConfig :: IO Params
loadConfig = do
  config <- (</> $(mkRelFile ".mida")) <$> getHomeDir
  exists <- doesFileExist config
  if exists
    then do
      let fconfig = fromAbsFile config
      params <- parseConfig fconfig <$> T.readFile fconfig
      case params of
        Right x -> return x
        Left  _ -> return M.empty
    else return M.empty

-- | Some information about the program.

opts :: ParserInfo Opts
opts =  info (helper <*> options)
      ( fullDesc
     <> progDesc "starts ALGA interpreter or patches given XML file"
     <> header "alga — algorithmic automation for various DAWs" )

-- | Description of command line options.

options :: Parser Opts
options = Opts
  <$> switch
  ( long "interactive"
  <> short 'i'
  <> help "Start ALGA in interactive mode" )
  <*> strOption
  ( long "backend"
  <> short 'B'
  <> metavar "DAW"
  <> value ""
  <> help "Specify which backend to use" )
  <*> option auto
  ( long "seed"
  <> short 's'
  <> metavar "SEED"
  <> value defaultSeed
  <> help ("Set seed for random numbers, default is " ++ show defaultSeed) )
  <*> option auto
  ( long "beats"
  <> short 'b'
  <> metavar "BEATS"
  <> value defaultBeats
  <> help ("Set total time in whole notes, default is " ++ show defaultBeats) )
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
