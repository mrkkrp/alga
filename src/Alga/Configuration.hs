--
-- Parse YAML configuration.
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

module Alga.Configuration
  ( AlgaConfig (..)
  , parseAlgaConfig
  , def )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Default
import Data.Yaml
import Numeric.Natural
import Path

-- | ALGA configuration.

data AlgaConfig = AlgaConfig
  { configBackend   :: String        -- ^ Selected backend (DAW dependent)
  , configPrevLen   :: Natural       -- ^ Length of preview principles
  , configSrcFile   :: Path Rel File -- ^ Name of current source file
  , configPrecision :: Double        -- ^ Precision for some REPL utilities
  , configPrompt    :: String        -- ^ REPL prompt
  , configVerbose   :: Bool          -- ^ Verbose mode?
  } deriving (Eq, Show)

instance Default AlgaConfig where
  def = AlgaConfig
    { configBackend   = "default"
    , configPrevLen   = 18
    , configSrcFile   = $(mkRelFile "foo.ga")
    , configPrecision = 0.01
    , configPrompt    = "> "
    , configVerbose   = True
    }

instance FromJSON AlgaConfig where
  parseJSON = withObject "ALGA configuration" $ \o -> do
    let ω f g n = do
          mval <- o .:? n
          case mval of
            Nothing -> return (f def)
            Just val -> g val
        ξ x = case parseRelFile x of
          Nothing -> fail $ "cannot parse relative path: " ++ show x
          Just path -> return path
        τ :: Int -> Parser Natural
        τ x = if x >= 0
                then return (fromIntegral x)
                else fail $ "the value must be non-negative: " ++ show x
    configBackend   <- ω configBackend   return "backend"
    configPrevLen   <- ω configPrevLen   τ      "prvlen"
    configSrcFile   <- ω configSrcFile   ξ      "src"
    configPrecision <- ω configPrecision return "precision"
    configPrompt    <- ω configPrompt    return "prompt"
    configVerbose   <- ω configVerbose   return "verbose"
    return AlgaConfig {..}

-- | Parse configuration from specified YAML file.

parseAlgaConfig :: MonadIO m => Path b File -> m (Either String AlgaConfig)
parseAlgaConfig path = liftIO $
  either (Left . prettyPrintParseException) Right
    <$> decodeFileEither (toFilePath path)
