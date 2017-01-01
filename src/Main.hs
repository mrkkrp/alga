--
-- Main module of ALGA describes logic of the program on top level.
--
-- Copyright © 2015–2017 Mark Karpov
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
import Control.Monad.IO.Class
import Data.FileEmbed
import Data.Text.Lazy (Text)
import Data.Version (showVersion)
import Formatting
import Numeric.Natural
import Options.Applicative
import Path.IO
import Paths_alga (version)
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
notice = $(embedStringFile "notice.txt")

-- | Longer copyright notice.

license :: Text
license = $(embedStringFile "license.txt")

-- | Read configuration file if present and run ALGA monad.

runAlga' :: Alga a -> IO ()
runAlga' e = do
  configFile <- resolveFile' ".alga.yaml"
  configExists <- doesFileExist configFile
  c <- if configExists
    then do
      econfig <- parseAlgaConfig configFile
      case econfig of
        Left msg -> liftIO (putStrLn msg) >> return def
        Right val -> return val
    else return def
  srcFile <- makeAbsolute (configSrcFile c)
  void $ runAlga e
    AlgaSt { stBackend = toBackend (configBackend c)
           , stPrevLen = configPrevLen c
           , stSrcFile = srcFile }
    AlgaCfg { cfgPrecision = configPrecision c
            , cfgPrompt    = configPrompt c
            , cfgVerbose   = configVerbose c }

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
