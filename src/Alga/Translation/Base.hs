-- -*- Mode: Haskell; -*-
--
-- In this module we describe how to prepare information about automation
-- tracks and then use specific (to DAW) arrow to patch XML file.
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

import Control.Monad.IO.Class
import Data.Maybe (isJust, maybeToList)
import Data.Ratio (numerator, denominator)
import qualified Data.Map.Lazy as M

import Text.Megaparsec
import Text.Megaparsec.String
import Text.XML.HXT.Core

import Alga.Language (AlgaEnv, setRandGen, getRefs, evalDef)
import Alga.Representation (autoDel)

type AlgaBackend = AutoMap -> IOSArrow XmlTree XmlTree
type AutoMap     = M.Map String AutoBatch
type AutoBatch   = M.Map AutoType AutoTrack

data AutoType
  = Volume
  | Mute
  | IGain
  | Pan
  | InsertSlot Int Int
  | SendSlot   Int Int
  | SynthParam Int
  deriving (Show, Eq, Ord)

data AutoTrack = AutoTrack
  { atVal :: [Double]
  , atDur :: [Double] }

nullTrack :: AutoTrack -> Bool
nullTrack AutoTrack { atVal = val, atDur = dur } = null val || null dur

totalDur :: AutoTrack -> Double
totalDur AutoTrack { atDur = dur } = sum dur

topDefs :: Monad m => AlgaEnv m [String]
topDefs = filter isTopRef <$> getRefs

patchAuto :: MonadIO m
          => Int -> Double -> FilePath -> AlgaBackend -> AlgaEnv m Int
patchAuto s b file exec = do
  setRandGen s
  refs <- getRefs
  amap <- M.fromListWith M.union . concat <$> mapM (toMap b) refs
  [status] <- liftIO . runX $
              readDocument [withValidate no] file
              >>> exec amap
              >>> writeDocument [withIndent yes] file
              >>> errorMsgStderr
              >>> getErrStatus
  return status

toMap :: Monad m => Double -> String -> AlgaEnv m [(String, AutoBatch)]
toMap b n =
  case parseTopRef n of
    Nothing      -> return []
    Just (r, at) -> fmap f . maybeToList <$> evalTrack b n
      where f x = (r, M.singleton at x)

evalTrack :: Monad m => Double -> String -> AlgaEnv m (Maybe AutoTrack)
evalTrack b valRef = do
  let durRef = valRef ++ durSuffix
  val <- fmap toFloat <$> evalDef valRef
  dur <- fmap toFloat <$> evalDef durRef
  return $ if null val || null dur
           then Nothing
           else Just $ slice b AutoTrack { atVal = val, atDur = dur }

slice :: Double -> AutoTrack -> AutoTrack
slice b AutoTrack { atVal = val, atDur = dur } =
  AutoTrack { atVal = take n val, atDur = take n dur }
  where n              = f 0 0 dur
        f !i _  []     = i
        f !i !a (x:xs) = if x + a >= b then succ i else f (succ i) (x + a) xs

toFloat :: Rational -> Double
toFloat x = n / d
  where n = fromIntegral $ numerator   x
        d = fromIntegral $ denominator x

-- Parsing

isTopRef :: String -> Bool
isTopRef = isJust . parseMaybe pTopRef'

parseTopRef :: String -> Maybe (String, AutoType)
parseTopRef = parseMaybe pTopRef

pTopRef' :: Parser (String, AutoType)
pTopRef' = pTopRef <* optional (string durSuffix)

pTopRef :: Parser (String, AutoType)
pTopRef = (,) <$> some (satisfy (/= autoDel)) <* char autoDel <*> pSuffix

pSuffix :: Parser AutoType
pSuffix =  try pVolume
       <|> try pMute
       <|> try pIGain
       <|> try pPan
       <|> try pInsert
       <|> try pSend
       <|> pSynth

pVolume :: Parser AutoType
pVolume = string "volume" *> pure Volume

pMute :: Parser AutoType
pMute = string "mute" *> pure Mute

pIGain :: Parser AutoType
pIGain = string "igain" *> pure IGain

pPan :: Parser AutoType
pPan = string "pan" *> pure Pan

pInsert :: Parser AutoType
pInsert = string "i" *> (InsertSlot <$> pNum <* string "_" <*> pNum)

pSend :: Parser AutoType
pSend = string "s" *> (SendSlot <$> pNum <* string "_" <*> pNum)

pSynth :: Parser AutoType
pSynth = string "p" *> (SynthParam <$> pNum)

pNum :: Parser Int
pNum = read <$> some digitChar

durSuffix :: String
durSuffix = "d"
