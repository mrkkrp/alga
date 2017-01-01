--
-- In this module we describe how to prepare information about automation
-- tracks and then use specific (to DAW) arrow to patch XML file.
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

{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Alga.Translation.Base
  ( AlgaBackend
  , AutoMap
  , AutoBatch
  , AutoType (..)
  , AutoTrack (..)
  , nullTrack
  , totalDur
  , topDefs
  , patchAuto )
where

import Alga.Language
import Alga.Representation (autoDel)
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Maybe (isJust, maybeToList)
import Data.Ratio (numerator, denominator)
import Numeric.Natural
import Path
import Text.Megaparsec
import Text.Megaparsec.String
import Text.XML.HXT.Core
import qualified Data.Map.Lazy as M
import qualified Text.Megaparsec.Lexer as L

-- | ALGA backend is a function that takes 'AutoMap' and returns arrow that
-- patches XML configuration accordingly for given DAW.

type AlgaBackend = AutoMap -> IOSArrow XmlTree XmlTree

-- | Collection of automation parameters per track. Tracks are identified by
-- names.

type AutoMap = Map String AutoBatch

-- | Collection of various aspects of automation track.

type AutoBatch = Map AutoType AutoTrack

-- | Aspect of automation, it includes simple volume or stereo panorama
-- controls as well as any arbitrary parameters supported by given
-- instrument or effect.

data AutoType
  = Volume             -- ^ Volume control
  | Mute               -- ^ “Mute” control
  | IGain              -- ^ Input gain control
  | Pan                -- ^ Stereo panorama control
  | InsertSlot Natural Natural -- ^ Index of insert slot\/index of parameter
  | SendSlot   Natural Natural -- ^ Index of send slot\/index of parameter
  | SynthParam Natural -- ^ Synth parameter index
  deriving (Show, Eq, Ord)

-- | Automation track is combination of control values and delta times
-- between them.

data AutoTrack = AutoTrack
  { atVal :: [Double]  -- ^ Value of parameter
  , atDur :: [Double]  -- ^ Delta time (duration of that “point”)
  } deriving (Eq, Show)

-- | Check if given track is empty.

nullTrack :: AutoTrack -> Bool
nullTrack AutoTrack {..} = null atVal || null atDur

-- | Calculate total duration of automation track.

totalDur :: AutoTrack -> Double
totalDur AutoTrack {..} = sum atDur

-- | Generate collection of “top-level” definitions (i.e. definitions that
-- are directly represent aspects of automation track, not auxiliary parts
-- in other definitions).

topDefs :: HasEnv m => m [String]
topDefs = filter isTopRef <$> getRefs

-- | Patch XML document containing automation settings for given DAW.

patchAuto :: (MonadIO m, HasEnv m)
  => Natural           -- ^ Seed for random number generator
  -> Double            -- ^ Duration as number of whole notes
  -> Path Abs File     -- ^ Path to file to patch
  -> AlgaBackend       -- ^ Backed (arrow that will patch XML)
  -> m Int             -- ^ Exit code
patchAuto s b fpath exec = do
  setRandGen s
  refs <- getRefs
  amap <- M.fromListWith M.union . concat <$>
    mapM (fmap maybeToList . toMap b) refs
  let file = fromAbsFile fpath
      uri = "file://" ++ n (g <$> file)
      g x = if x == '\\' then '/' else x
      n x = if head x /= '/' then '/' : x else x
  head <$> (liftIO . runX $
    readDocument [withValidate no] uri  >>>
    exec amap                           >>>
    writeDocument [withIndent yes] file >>>
    errorMsgStderr                      >>>
    getErrStatus)

-- | Calculate an automation aspect given name of definition.

toMap :: HasEnv m
  => Double            -- ^ Duration as number of whole notes
  -> String            -- ^ Name of definition of automation aspect
  -> m (Maybe (String, AutoBatch)) -- ^ Automation (maybe)
toMap b n =
  case parseTopRef n of
    Nothing      -> return Nothing
    Just (r, at) -> fmap f <$> evalTrack b n
      where f x = (r, M.singleton at x)

-- | Evaluate automation aspect if it's defined.

evalTrack :: HasEnv m
  => Double            -- ^ Duration as number of whole notes
  -> String            -- ^ Name of definition
  -> m (Maybe AutoTrack) -- ^ Automation track (maybe)
evalTrack b valRef = do
  let durRef = valRef ++ durSuffix
  val <- fmap toFloat <$> evalDef valRef
  dur <- fmap toFloat <$> evalDef durRef
  return $ if null val || null dur
    then Nothing
    else Just $ slice b AutoTrack { atVal = val, atDur = dur }

-- | Get sufficient part of infinite stream of numbers in automation track
-- making it finite.

slice
  :: Double            -- ^ Duration as number of whole notes
  -> AutoTrack         -- ^ Infinite automation track
  -> AutoTrack         -- ^ Finite automation track
slice b AutoTrack {..} =
  let n              = f 0 0 atDur
      f !i _  []     = i
      f !i !a (x:xs) = if x + a >= b then succ i else f (succ i) (x + a) xs
  in AutoTrack { atVal = take n atVal, atDur = take n atDur }

-- | Convert non-negative rational number into floating point value.

toFloat :: NRatio -> Double
toFloat x = fromIntegral (numerator x) / fromIntegral (denominator x)

----------------------------------------------------------------------------
-- Parsing

-- | Check if given definition name belongs to “top-level”.

isTopRef :: String -> Bool
isTopRef = isJust . parseMaybe pTopRef'

-- | Parser of top-level definitions. Returns base name of corresponding
-- automation aspect.

parseTopRef :: String -> Maybe (String, AutoType)
parseTopRef = parseMaybe pTopRef

pTopRef' :: Parser (String, AutoType)
pTopRef' = pTopRef <* optional (string durSuffix)

pTopRef :: Parser (String, AutoType)
pTopRef = (,) <$> some (satisfy (/= autoDel)) <* char autoDel <*> pSuffix

pSuffix :: Parser AutoType
pSuffix
  =   try pVolume
  <|> try pMute
  <|> try pIGain
  <|> try pPan
  <|> try pInsert
  <|> try pSend
  <|> pSynth

pVolume :: Parser AutoType
pVolume = Volume <$ string "volume"

pMute :: Parser AutoType
pMute = Mute <$ string "mute"

pIGain :: Parser AutoType
pIGain = IGain <$ string "igain"

pPan :: Parser AutoType
pPan = Pan <$ string "pan"

pInsert :: Parser AutoType
pInsert = string "i" *> (InsertSlot <$> pNum <* string "_" <*> pNum)

pSend :: Parser AutoType
pSend = string "s" *> (SendSlot <$> pNum <* string "_" <*> pNum)

pSynth :: Parser AutoType
pSynth = string "p" *> (SynthParam <$> pNum)

pNum :: Parser Natural
pNum = fromIntegral <$> L.integer

durSuffix :: String
durSuffix = "d"
