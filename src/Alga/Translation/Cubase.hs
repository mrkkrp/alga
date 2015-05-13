-- -*- Mode: Haskell; -*-
--
-- Patching of XML files, Cubase backend.
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

module Alga.Translation.Cubase (cubaseBackend) where

import Control.Arrow
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as M

import Control.Arrow.ArrowTree
import Text.XML.HXT.Core

import Alga.Translation.Base

infixl 7 ++=
infixr 5 />/

cubaseBackend :: AutoMap -> IOSArrow XmlTree XmlTree
cubaseBackend m = configSysVars [withNoEmptyElemFor nonEmptyElts]
                  >>> "tracklist" />/ inList "track" ("obj" />/ procTrack m)

procTrack :: ArrowXml a => AutoMap -> a XmlTree XmlTree
procTrack m = mthis (inClass "MAutomationNode" . procAuto)
              $< (trackName >>> arr (`M.lookup` m))

procAuto :: ArrowXml a => AutoBatch -> a XmlTree XmlTree
procAuto batch = setInt "Expanded" 1 >>> deleteOld += mkTracks
    where deleteOld = processChildren (none `when` deletable)
          deletable = isList "Tracks" <+> isClass "MAutomationTrack"
          mkTracks  = M.foldlWithKey' f (mkList "Tracks" "obj" []) batch
          f a k t   = a ++= g k t
          g k       =
              case k of
                Volume      -> volumeEvent
                Mute        -> muteEvent
                IGain       -> igainEvent
                Pan         -> panEvent
                InsSlot _ _ -> undefined
                InstParam _ -> undefined

volumeEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
volumeEvent = addEvent 1025 4 Nothing . fixRange

muteEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
muteEvent = addEvent 1027 5 Nothing

igainEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
igainEvent = addEvent 4099 4 Nothing . fixRange

panEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
panEvent = addEvent 4201 6 (Just "Panner")

-- insEvent :: ArrowXml a => String -> AutoTrack -> a XmlTree XmlTree
-- insEvent dn = addEvent 4202 4 (Just dn)

addEvent :: ArrowXml a => Int -> Int -> Maybe String ->
            AutoTrack -> a XmlTree XmlTree
addEvent tag flags dn t =
    mkObj (Just "MAutomationTrackEvent") na na
              [ mkInt   "Flags"  32
              , mkFloat "Start"  0
              , mkFloat "Length" (durFactor * totalDur t)
              , mkObj (Just "MAutoListNode") (Just "Node") na
                [ mkMember "Domain"
                  [ mkInt "Type" 0 ]
                , mkList "Events" "obj" (genEvents t) ]
              , mkAutoTrack dn
              , mkInt "Height" 42
              , mkInt "Tag"    tag
              , mkInt "TrackFlags" flags ]

mkAutoTrack :: ArrowXml a => Maybe String -> a XmlTree XmlTree
mkAutoTrack dn = ifA (deep $ isAT dn) mkLink mkCmpl
    where mkCmpl = mkObj (Just "MAutomationTrack") name (Just $ genID dn)
                   [ mkInt "Connection Type" (maybe 2 (const 7) dn)
                   , mnone (mkString "Device Name") dn
                   , mkInt "Read" 1
                   , mkInt "Write" 0 ]
          mkLink    = mkObj na name (Just (genID dn)) []
          name      = Just "Track Device"

genEvents :: ArrowXml a => AutoTrack -> [a XmlTree XmlTree]
genEvents AutoTrack { atVal = val, atDur = dur } = zipWith f val (fixDur dur)
    where f v d = mkObj (Just "MParamEvent") na na
                  [mkFloat "Start" d, mkFloat "Value" v]

fixRange :: AutoTrack -> AutoTrack
fixRange AutoTrack { atVal = val, atDur = dur} = AutoTrack (f <$> val) dur
    where f = (* 0.78908658027648926)

fixDur :: [Double] -> [Double]
fixDur = fmap (* durFactor) . reverse . tail . foldl' f []
    where f []       a = [a,0]
          f xs@(x:_) a = x + a : xs

genID :: Maybe String -> String
genID = show . (+ 13) . maybe 0 (sum . zipWith (+) [255,510..] . fmap ord)

-- Element Creation

mkObj :: ArrowXml a => Maybe String -> Maybe String -> Maybe String ->
         [a XmlTree XmlTree] -> a XmlTree XmlTree
mkObj c n i = mkelem "obj"
              [ mnone (sattr "class") c
              , mnone (sattr "name")  n
              , sattr "ID" (fromMaybe "7" i) ]

mkMember :: ArrowXml a => String -> [a XmlTree XmlTree] -> a XmlTree XmlTree
mkMember name = mkelem "member" [sattr "name" name]

mkList :: ArrowXml a => String -> String -> [a XmlTree XmlTree] ->
          a XmlTree XmlTree
mkList name t = mkelem "list" [sattr "name" name, sattr "type" t]

mkString :: ArrowXml a => String -> String -> a XmlTree XmlTree
mkString n v = mkNamedVal "string" n v += sattr "wide" "true"

mkInt :: ArrowXml a => String -> Int -> a XmlTree XmlTree
mkInt n v = mkNamedVal "int" n (show v)

mkFloat :: ArrowXml a => String -> Double -> a XmlTree XmlTree
mkFloat n v = mkNamedVal "float" n (show v)

mkNamedVal :: ArrowXml a => String -> String -> String -> a XmlTree XmlTree
mkNamedVal t n v = mkelem t [sattr "name" n, sattr "value" v] []

-- Get/Set/Access Information

trackName :: ArrowXml a => a XmlTree String
trackName = catA (isClass <$> cs) /> isClass "MListNode" /> getStr "Name"
    where cs = ["MAudioTrackEvent","MInstrumentTrackEvent","MDeviceTrackEvent"]

getStr :: ArrowXml a => String -> a XmlTree String
getStr name = isElem >>> hasName "string" >>>
              hasAttrValue "name" (== name) >>> getAttrValue "value"

setInt :: ArrowXml a => String -> Int -> a XmlTree XmlTree
setInt name val = processChildren $ setVal `when` rightInt
    where setVal   = addAttr "value" (show val)
          rightInt = isElem >>> hasName "int" >>> hasAttrValue "name" (== name)

-- Special Primitives

isAT :: ArrowXml a => Maybe String -> a XmlTree XmlTree
isAT i = isClass "MAutomationTrack" >>> hasAttrValue "ID" (== genID i)

inList :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
inList name action = processChildren $ action `when` isList name

isList :: ArrowXml a => String -> a XmlTree XmlTree
isList name = isElem >>> hasName "list" >>> hasAttrValue "name" (== name)

inClass :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
inClass cls action = processChildren $ action `when` isClass cls

isClass :: ArrowXml a => String -> a XmlTree XmlTree
isClass cls = isElem >>> hasName "obj" >>> hasAttrValue "class" (== cls)

mthis :: ArrowXml a => (b -> a XmlTree XmlTree) -> Maybe b -> a XmlTree XmlTree
mthis = maybe this

mnone :: ArrowXml a => (b -> a XmlTree XmlTree) -> Maybe b -> a XmlTree XmlTree
mnone = maybe none

(++=) :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree ->
         a XmlTree XmlTree
a ++= b = a += (a >>> b)

(/>/) :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
name />/ action = processChildren $ action `when` (isElem >>> hasName name)

-- Constants

nonEmptyElts :: [String]
nonEmptyElts = ["bin","member","obj","list"]

na :: Maybe a
na = Nothing

durFactor :: Double
durFactor = 1920
