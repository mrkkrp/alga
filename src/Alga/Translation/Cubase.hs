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
import qualified Data.Map.Lazy as M

import Control.Arrow.ArrowTree
import Text.XML.HXT.Core

import Alga.Translation.Base

cubaseBackend :: AutoMap -> IOSArrow XmlTree XmlTree
cubaseBackend m = configSysVars [withNoEmptyElemFor ["bin","member"]]
                  >>> "tracklist" />/ inList "track" ("obj" />/ procTrack m)

procTrack :: ArrowXml a => AutoMap -> a XmlTree XmlTree
procTrack m = maybe' (inClass "MAutomationNode" . procAuto)
              $< (trackName >>> arr (`M.lookup` m))

procAuto :: ArrowXml a => AutoBatch -> a XmlTree XmlTree
procAuto b = setInt "Expanded" 1 >>> inList "Tracks" (replaceChildren  events)
    where events = maybe' volumeEvent (abVolume b) >>>
                   maybe' muteEvent   (abMute   b) >>>
                   maybe' igainEvent  (abIGain  b)

volumeEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
volumeEvent = addEvent 1025 4 . fixRange

muteEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
muteEvent = addEvent 1027 5

igainEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
igainEvent = addEvent 4099 4 . fixRange

addEvent :: ArrowXml a => Int -> Int -> AutoTrack -> a XmlTree XmlTree
addEvent tag flags t =
    mkObj (Just "MAutomationTrackEvent") Nothing
              [ mkInt   "Flags"  32
              , mkFloat "Start"  0
              , mkFloat "Length" (durFactor * totalDur t)
              , mkObj (Just "MAutoListNode") (Just "Node")
                [ mkMember "Domain"
                  [ mkInt "Type" 0
                  , mkObj Nothing (Just "Tempo Track") []
                  , mkObj Nothing (Just "Signature Track") [] ]
                , mkList "Events" "obj" (genEvents t) ]
              , mkObj (Just "MAutomationTrack") (Just "Track Device")
                [ mkInt "Connection Type" 2
                , mkInt "Read" 1
                , mkInt "Write" 0 ]
              , mkInt "Height" 42
              , mkInt "Tag"    tag
              , mkInt "TrackFlags" flags ]

genEvents :: ArrowXml a => AutoTrack -> [a XmlTree XmlTree]
genEvents AutoTrack { atVal = val, atDur = dur } = zipWith f val (fixDur dur)
    where f v d = mkObj (Just "MParamEvent") Nothing
                  [mkFloat "Start" d, mkFloat "Value" v]

fixRange :: AutoTrack -> AutoTrack
fixRange AutoTrack { atVal = val, atDur = dur} = AutoTrack (f <$> val) dur
    where f = (* 0.78908658027648926)

fixDur :: [Double] -> [Double]
fixDur = fmap (* durFactor) . reverse . tail . foldl' f []
    where f []       a = [a,0]
          f xs@(x:_) a = x + a : xs

-- Element Creation

mkObj :: ArrowXml a => Maybe String -> Maybe String -> [a XmlTree XmlTree] ->
         a XmlTree XmlTree
mkObj c n = mkelem "obj" [sattr' "class" c, sattr' "name" n]
    where sattr' x = maybe none (sattr x)

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
trackName = alt isClass cs /> isClass "MListNode" /> getStr "Name"
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

maybe' :: ArrowXml a => (b -> a XmlTree XmlTree) -> Maybe b -> a XmlTree XmlTree
maybe' = maybe this

alt :: ArrowXml a => (b -> a XmlTree XmlTree) -> [b] -> a XmlTree XmlTree
alt a = foldr ((<+>) . a) none

infixr 5 />/

(/>/) :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
name />/ action = processChildren $ action `when` (isElem >>> hasName name)

-- Constants

durFactor :: Double
durFactor = 1920
