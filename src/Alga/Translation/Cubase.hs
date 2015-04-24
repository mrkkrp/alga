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

{-# LANGUAGE Arrows #-}

module Alga.Translation.Cubase (cubaseBackend) where

import Control.Arrow
import qualified Data.Map.Lazy as M

import Control.Arrow.ArrowTree
import Text.XML.HXT.Core

import Alga.Translation.Base

infixr 5 />/

cubaseBackend :: AutoMap -> IOSArrow XmlTree XmlTree
cubaseBackend m = "tracklist" />/ "list" />/ "obj" />/ procTrack m

procTrack :: (ArrowXml a, ArrowChoice a) => AutoMap -> a XmlTree XmlTree
procTrack m = (trackName >>> arr (`M.lookup` m)) &&& this >>>
             proc (x, tree) ->
                  case x of
                    Nothing -> this     -< tree
                    Just b  -> procAuto -< (b, tree)

procAuto :: ArrowXml a => a (AutoBatch, XmlTree) XmlTree
procAuto = arr snd >>> inClass "MAutomationNode"
           (replaceChildren $ txt "Hello, Cubase!")

trackName :: ArrowXml a => a XmlTree String
trackName = alt ofClass cs /> ofClass "MListNode" /> getStr "Name"
    where cs = ["MAudioTrackEvent","MInstrumentTrackEvent","MDeviceTrackEvent"]

alt :: ArrowXml a => (b -> a XmlTree XmlTree) -> [b] -> a XmlTree XmlTree
alt a = foldr ((<+>) . a) this

inClass :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
inClass cls action = processChildren $ action `when` ofClass cls

ofClass :: ArrowXml a => String -> a XmlTree XmlTree
ofClass cls = isElem >>> hasName "obj" >>> hasAttrValue "class" (== cls)

getStr :: ArrowXml a => String -> a XmlTree String
getStr name = isElem >>> hasName "string" >>>
              hasAttrValue "name" (== name) >>> getAttrValue "value"

(/>/) :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
name />/ action = processChildren $ action `when` (isElem >>> hasName name)
