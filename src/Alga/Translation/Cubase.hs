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
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as M

import Control.Arrow.ArrowTree
import Text.XML.HXT.Core

import Alga.Translation.Base

infixl 7 ++=
infixr 8 <~
infixr 5 />/

cubaseBackend :: AutoMap -> IOSArrow XmlTree XmlTree
cubaseBackend m = configSysVars [withNoEmptyElemFor nonEmptyElts]
                  >>> "tracklist" />/ inList "track" ("obj" />/ procTrack m)

procTrack :: ArrowXml a => AutoMap -> a XmlTree XmlTree
procTrack m = mthis (inClass "MAutomationNode" . procAuto)
              $< (trackName >>> arr (`M.lookup` m))

procAuto :: ArrowXml a => AutoBatch -> a XmlTree XmlTree
procAuto AutoBatch { abVolume = volume
                   , abMute   = mute
                   , abIGain  = igain }
    = setInt "Expanded" 1 >>> deleteOld += mkTracks
    where deleteOld = processChildren (none `when` deletable)
          deletable = isList "Tracks" <+> isClass "MAutomationTrack"
          mkTracks  = mkList "Tracks" "obj" []
                      ++= volumeEvent <~ volume
                      ++= muteEvent   <~ mute
                      ++= igainEvent  <~ igain

volumeEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
volumeEvent = addEvent 1025 4 . fixRange

muteEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
muteEvent = addEvent 1027 5

igainEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
igainEvent = addEvent 4099 4 . fixRange

addEvent :: ArrowXml a => Int -> Int -> AutoTrack -> a XmlTree XmlTree
addEvent tag flags t =
    mkObj (Just "MAutomationTrackEvent") na na
              [ mkInt   "Flags"  32
              , mkFloat "Start"  0
              , mkFloat "Length" (durFactor * totalDur t)
              , mkObj (Just "MAutoListNode") (Just "Node") na
                [ mkMember "Domain"
                  [ mkInt "Type" 0 ]
                , mkList "Events" "obj" (genEvents t) ]
              , mkAutoTrack
              , mkInt "Height" 42
              , mkInt "Tag"    tag
              , mkInt "TrackFlags" flags ]

mkAutoTrack :: ArrowXml a => a XmlTree XmlTree
mkAutoTrack = ifA present mkLink mkComplete
    where present    = deep (isClass "MAutomationTrack")
          mkComplete = mkObj (Just "MAutomationTrack") name i
                       [ mkInt "Connection Type" 2
                       , mkInt "Read" 1
                       , mkInt "Write" 0 ]
          mkLink     = mkObj na name i []
          name       = Just "Track Device"
          i          = Just "00000013"

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

-- Element Creation

mkObj :: ArrowXml a => Maybe String -> Maybe String -> Maybe String ->
         [a XmlTree XmlTree] -> a XmlTree XmlTree
mkObj c n i = mkelem "obj"
              [ mnone (sattr "class") c
              , mnone (sattr "name")  n
              , sattr "ID" (fromMaybe "00000007" i) ]

mkMember :: ArrowXml a => String -> [a XmlTree XmlTree] -> a XmlTree XmlTree
mkMember name = mkelem "member" [sattr "name" name]

mkList :: ArrowXml a => String -> String -> [a XmlTree XmlTree] ->
          a XmlTree XmlTree
mkList name t = mkelem "list" [sattr "name" name, sattr "type" t]

mkInt :: ArrowXml a => String -> Int -> a XmlTree XmlTree
mkInt = mkNamedVal "int"

mkFloat :: ArrowXml a => String -> Double -> a XmlTree XmlTree
mkFloat = mkNamedVal "float"

mkNamedVal :: (ArrowXml a, Show b) => String -> String -> b -> a XmlTree XmlTree
mkNamedVal t n v = mkelem t [sattr "name" n, sattr "value" (show v)] []

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

(<~) :: ArrowXml a => (b -> a XmlTree XmlTree) -> Maybe b -> a XmlTree XmlTree
a <~ m = mnone a m

(/>/) :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
name />/ action = processChildren $ action `when` (isElem >>> hasName name)

-- Constants

nonEmptyElts :: [String]
nonEmptyElts = ["bin","member","obj","list"]

na :: Maybe a
na = Nothing

durFactor :: Double
durFactor = 1920
