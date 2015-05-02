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

-- {-# LANGUAGE Arrows #-}

module Alga.Translation.Cubase (cubaseBackend) where

import Control.Arrow
import qualified Data.Map.Lazy as M

import Control.Arrow.ArrowTree
import Text.XML.HXT.Core

import Alga.Translation.Base

infixr 5 />/

cubaseBackend :: AutoMap -> IOSArrow XmlTree XmlTree
cubaseBackend m = "tracklist" />/ inList "track" ("obj" />/ procTrack m)

procTrack :: ArrowXml a => AutoMap -> a XmlTree XmlTree
procTrack m = maybe' (inClass "MAutomationNode" . procAuto)
              $< (trackName >>> arr (`M.lookup` m))

procAuto :: ArrowXml a => AutoBatch -> a XmlTree XmlTree
procAuto b = setInt "Expanded" 1 >>> inList "Tracks" events
    where events = maybe' volumeEvent (abVolume b) >>>
                   maybe' muteEvent   (abMute   b) >>>
                   maybe' igainEvent  (abIGain  b)

volumeEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
volumeEvent _ = this -- fix me

muteEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
muteEvent _ = this -- fix me

igainEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
igainEvent _ = this -- fix me

trackName :: ArrowXml a => a XmlTree String
trackName = alt isClass cs /> isClass "MListNode" /> getStr "Name"
    where cs = ["MAudioTrackEvent","MInstrumentTrackEvent","MDeviceTrackEvent"]

maybe' :: ArrowXml a => (b -> a XmlTree XmlTree) -> Maybe b -> a XmlTree XmlTree
maybe' = maybe this

alt :: ArrowXml a => (b -> a XmlTree XmlTree) -> [b] -> a XmlTree XmlTree
alt a = foldr ((<+>) . a) none

inList :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
inList name action = processChildren $ action `when` isList name

isList :: ArrowXml a => String -> a XmlTree XmlTree
isList name = isElem >>> hasName "list" >>> hasAttrValue "name" (== name)

inClass :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
inClass cls action = processChildren $ action `when` isClass cls

isClass :: ArrowXml a => String -> a XmlTree XmlTree
isClass cls = isElem >>> hasName "obj" >>> hasAttrValue "class" (== cls)

setInt :: ArrowXml a => String -> Int -> a XmlTree XmlTree
setInt name val = processChildren $ setVal `when` rightInt
    where setVal   = addAttr "value" (show val)
          rightInt = isElem >>> hasName "int" >>> hasAttrValue "name" (== name)

getStr :: ArrowXml a => String -> a XmlTree String
getStr name = isElem >>> hasName "string" >>>
              hasAttrValue "name" (== name) >>> getAttrValue "value"

(/>/) :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
name />/ action = processChildren $ action `when` (isElem >>> hasName name)
