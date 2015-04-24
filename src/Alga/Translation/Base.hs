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
    , AutoBatch
    , AutoTrack
    , abVolume
    , abMute
    , abIGain
    , atVal
    , atDur
    , topDefs
    , patchAuto )
where

import Control.Monad.IO.Class
import Data.List (nub, isSuffixOf)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Text.XML.HXT.Core

import Alga.Language (AlgaEnv, setRandGen, getRefs, evalDef)
import Alga.Representation (autoDel)

type AutoMap = M.Map String AutoBatch

data AutoBatch = AutoBatch
    { abVolume :: Maybe AutoTrack
    , abMute   :: Maybe AutoTrack
    , abIGain  :: Maybe AutoTrack }

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
              readDocument [] file
              >>> exec amap
              >>> writeDocument [withIndent yes] file
              >>> errorMsgStderr
              >>> getErrStatus
  return status

makeBatch :: Monad m => Double -> String -> AlgaEnv m (String, AutoBatch)
makeBatch b t = do
  let eval' = evalTrack b t
  volume <- eval' atnVolume
  mute   <- eval' atnMute
  igain  <- eval' atnIGain
  return (t, AutoBatch
               { abVolume = volume
               , abMute   = mute
               , abIGain  = igain })

evalTrack :: Monad m => Double -> String -> String -> AlgaEnv m (Maybe AutoTrack)
evalTrack b t a = do
  let valRef = t ++ [autoDel] ++ a
      durRef = valRef ++ durSuffix
  val <- evalDef valRef
  dur <- evalDef durRef
  return $ if null val || null dur
           then Nothing
           else Just $ slice b AutoTrack { atVal = val, atDur = dur }

slice :: Double -> AutoTrack -> AutoTrack
slice b AutoTrack { atVal = val, atDur = dur } =
    AutoTrack { atVal = take n val, atDur = take n dur }
    where n              = f 0 0 dur
          f !i _  []     = i
          f !i !a (x:xs) = if x + a >= b then succ i else f (succ i) (x + a) xs

topRef :: String -> [String]
topRef name = [t | a `S.member` topRefSuffixes]
    where (t, a) = break (== autoDel) name

atnVolume :: String
atnVolume = "volume"

atnMute :: String
atnMute = "mute"

atnIGain :: String
atnIGain = "igain"

durSuffix :: String
durSuffix = "d"

topRefSuffixes :: AutoSet
topRefSuffixes = S.map (autoDel:) (S.fromList raw)
    where raw = [atnVolume,atnMute,atnIGain] >>= \x -> [x, x ++ durSuffix]
