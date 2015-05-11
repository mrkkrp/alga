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

{-# LANGUAGE BangPatterns #-}

module Alga.Translation.Base
    ( AutoMap
    , AutoBatch (..)
    , AutoTrack (..)
    , topDefs
    , patchAuto
    , totalDur )
where

import Control.Monad.IO.Class
import Data.List (nub, isSuffixOf)
import Data.Ratio (numerator, denominator)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Text.XML.HXT.Core

import Alga.Language (AlgaEnv, setRandGen, getRefs, evalDef)
import Alga.Representation (autoDel)

type AutoMap = M.Map String AutoBatch

data AutoBatch = AutoBatch
    { abVolume :: Maybe AutoTrack , abIns2 :: Maybe AutoTrack
    , abMute   :: Maybe AutoTrack , abIns3 :: Maybe AutoTrack
    , abIGain  :: Maybe AutoTrack , abIns4 :: Maybe AutoTrack
    , abPan    :: Maybe AutoTrack , abIns5 :: Maybe AutoTrack
    , abIns0   :: Maybe AutoTrack , abIns6 :: Maybe AutoTrack
    , abIns1   :: Maybe AutoTrack , abIns7 :: Maybe AutoTrack }

data AutoTrack = AutoTrack
    { atVal :: [Double]
    , atDur :: [Double] }

type AutoSet = S.Set String

topDefs :: Monad m => AlgaEnv m [String]
topDefs = filter f <$> getRefs
    where f x = any (`isSuffixOf` x) topRefSuffixes

patchAuto :: MonadIO m => Int -> Double -> FilePath ->
             (AutoMap -> IOSArrow XmlTree XmlTree) -> AlgaEnv m Int
patchAuto s b file exec = do
  setRandGen s
  refs <- nub . (>>= topRef) <$> getRefs
  amap <- M.fromList <$> mapM (makeBatch b) refs
  [status] <- liftIO . runX $
              readDocument [withValidate no] file
              >>> exec amap
              >>> writeDocument [withIndent yes] file
              >>> errorMsgStderr
              >>> getErrStatus
  return status

totalDur :: AutoTrack -> Double
totalDur AutoTrack { atDur = dur } = sum dur

makeBatch :: Monad m => Double -> String -> AlgaEnv m (String, AutoBatch)
makeBatch b t = do
  let eval' = evalTrack b t
  volume <- eval' atnVolume
  mute   <- eval' atnMute
  igain  <- eval' atnIGain
  pan    <- eval' atnPan
  ins0   <- eval' atnIns0
  ins1   <- eval' atnIns1
  ins2   <- eval' atnIns2
  ins3   <- eval' atnIns3
  ins4   <- eval' atnIns4
  ins5   <- eval' atnIns5
  ins6   <- eval' atnIns6
  ins7   <- eval' atnIns7
  return (t, AutoBatch
               { abVolume = volume , abIns2 = ins2
               , abMute   = mute   , abIns3 = ins3
               , abIGain  = igain  , abIns4 = ins4
               , abPan    = pan    , abIns5 = ins5
               , abIns0   = ins0   , abIns6 = ins6
               , abIns1   = ins1   , abIns7 = ins7 })

evalTrack :: Monad m => Double -> String -> String -> AlgaEnv m (Maybe AutoTrack)
evalTrack b t a = do
  let valRef = t ++ [autoDel] ++ a
      durRef = valRef ++ durSuffix
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

topRef :: String -> [String]
topRef name = [t | a `S.member` topRefSuffixes]
    where (t, a) = break (== autoDel) name

topRefSuffixes :: AutoSet
topRefSuffixes = S.map (autoDel:) (S.fromList raw)
    where raw = [ atnVolume , atnIns2
                , atnMute   , atnIns3
                , atnIGain  , atnIns4
                , atnPan    , atnIns5
                , atnIns0   , atnIns6
                , atnIns1   , atnIns7 ]
                >>= \x -> [x, x ++ durSuffix]

atnVolume :: String
atnVolume = "volume"

atnMute :: String
atnMute = "mute"

atnIGain :: String
atnIGain = "igain"

atnPan :: String
atnPan = "pan"

atnIns0 :: String
atnIns0 = "ins0"

atnIns1 :: String
atnIns1 = "ins1"

atnIns2 :: String
atnIns2 = "ins2"

atnIns3 :: String
atnIns3 = "ins3"

atnIns4 :: String
atnIns4 = "ins4"

atnIns5 :: String
atnIns5 = "ins5"

atnIns6 :: String
atnIns6 = "ins6"

atnIns7 :: String
atnIns7 = "ins7"

durSuffix :: String
durSuffix = "d"
