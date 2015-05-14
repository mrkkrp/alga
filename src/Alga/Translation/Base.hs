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

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Alga.Translation.Base
    ( AutoMap
    , AutoBatch
    , AutoType (..)
    , AutoTrack (..)
    , topDefs
    , patchAuto
    , nullTrack
    , totalDur )
where

import Control.Monad.IO.Class
import Data.List (nub, isSuffixOf)
import Data.Maybe (maybeToList)
import Data.Ratio (numerator, denominator)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Text.XML.HXT.Core

import Alga.Language (AlgaEnv, setRandGen, getRefs, evalDef)
import Alga.Representation (autoDel)

type AutoMap   = M.Map String AutoBatch
type AutoBatch = M.Map AutoType AutoTrack

data AutoType
    = Volume
    | Mute
    | IGain
    | Pan
    | InsertSlot Int Int
    | SendSlot Int Int
    | SynthParam Int
    deriving (Show, Eq, Ord)

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

nullTrack :: AutoTrack -> Bool
nullTrack AutoTrack { atVal = val, atDur = dur } = null val || null dur

totalDur :: AutoTrack -> Double
totalDur AutoTrack { atDur = dur } = sum dur

makeBatch :: Monad m => Double -> String -> AlgaEnv m (String, AutoBatch)
makeBatch b t = (t,) . M.fromList . (>>= maybeToList) <$>
                mapM (evalTrack b t) autoKeys

evalTrack :: Monad m => Double -> String -> String ->
             AlgaEnv m (Maybe (AutoType, AutoTrack))
evalTrack b t a = do
  let valRef = t ++ [autoDel] ++ a
      durRef = valRef ++ durSuffix
  val <- fmap toFloat <$> evalDef valRef
  dur <- fmap toFloat <$> evalDef durRef
  return $ (,) <$> autoType a <*>
         if null val || null dur
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
    where raw = autoKeys >>= \x -> [x, x ++ durSuffix]

autoType :: String -> Maybe AutoType
autoType i = M.lookup i autoMap

autoMap :: M.Map String AutoType
autoMap = M.fromList (basicMap ++ insertMap ++ sendMap ++ synthMap)
    where basicMap  = [ ("volume", Volume) , ("igain",  IGain)
                      , ("mute",   Mute)   , ("pan",    Pan) ]
          insertMap = [ ("i" ++ show n ++ "_" ++ show s, InsertSlot n s)
                      | n <- [0..7], s <- [0..99] ]
          sendMap   = [ ("s" ++ show n ++ "_" ++ show s, SendSlot n s)
                      | n <- [0..7], s <- [0..9] ]
          synthMap  = [ ("p" ++ show s, SynthParam s) | s <- [0..99] ]

autoKeys :: [String]
autoKeys = M.keys autoMap

durSuffix :: String
durSuffix = "d"
