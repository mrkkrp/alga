--
-- Patching of XML files, Cubase backend.
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

{-# LANGUAGE RecordWildCards #-}

module Alga.Translation.Cubase (cubaseBackend) where

import Alga.Translation.Base
import Control.Arrow
import Control.Arrow.ArrowTree
import Data.Bool (bool)
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric.Natural
import Text.XML.HXT.Core
import qualified Data.Map.Lazy as M

-- | Cubase backend for XML configuration patching. This module is result of
-- my reverse engineering sessions with Cubase 5 XML-exported tracks. Due to
-- heavy use of arrows, this is a very dense piece of code that may be
-- difficult to read and understand. HXT documentation and general arrow
-- tutorials should help here.

cubaseBackend :: AlgaBackend
cubaseBackend m =
  configSysVars [withNoEmptyElemFor nonEmptyElts] >>>
  "tracklist" />/ inList "track" ("obj" />/ procTrack m)

-- | Process a track. This first identifies tracks for which we have
-- definitions in given 'AutoMap'. Every found definition gets its
-- 'ritualVolumeEvent', because Cubase likes to have empty volume automation
-- track in all cases (when we already have volume automation nothing is
-- added, of course).
--
-- Then for tracks that are found in XML document and have at the save time
-- automation to add, we need to find GUIDs. 'insertGuids' gets list of
-- GUIDs for insert slots, while 'synthGuids' does the same for synth
-- slots. Then this together with 'AutoBatch' is passed to 'procAuto'.

procTrack :: ArrowXml a
  => AutoMap           -- ^ Collection of automation parameters
  -> a XmlTree XmlTree -- ^ Arrow transforming a track
procTrack amap = mthis editTrack $< (trackName >>> arr prepBatch)
  where editTrack ab = g ab $<< insertGuids &&& synthGuids
        g ab is ps   = inClass "MAutomationNode" (procAuto ab is ps)
        prepBatch x  = (`M.union` ritualVolumeEvent) <$> (x `M.lookup` amap)

-- | Process automation track. First we add integer parameter “Expanded”
-- with value 1, so automation tracks are visible and obvious then patches
-- docuement is loaded. Then we delete existing automation tracks (they go
-- through 'none' arrow), and add various automation events one by one.

procAuto :: ArrowXml a
  => AutoBatch         -- ^ Aspects of an automation track
  -> [String]          -- ^ Insert slots GUIDs
  -> [String]          -- ^ Synth slots GUIDs
  -> a XmlTree XmlTree -- ^ Arrow transforming automation track
procAuto batch is ps = setInt "Expanded" 1 >>> deleteOld += mkTracks
  where deleteOld = processChildren (none `when` deletable)
        deletable = isList "Tracks" <+> isClass "MAutomationTrack"
        mkTracks  = M.foldlWithKey' f (mkList "Tracks" "obj" []) batch
        f a k t   = a ++= g k t
        g k       =
          case k of
            Volume         -> volumeEvent
            Mute           -> muteEvent
            IGain          -> igainEvent
            Pan            -> panEvent
            InsertSlot n s -> mnone' (`iEvent` s) (genInsertDN is n)
            SendSlot   n s -> sendEvent (genSendDN n) s
            SynthParam   s -> mnone' (`iEvent` s) (genSynthDN ps)
        mnone'    = maybe (const none)

-- | Calculate volume automation event.

volumeEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
volumeEvent = addEvent 1025 4 Nothing . fixRange

-- | Calculate mute automation event.

muteEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
muteEvent = addEvent 1027 5 Nothing

-- | Calculate input gain automation event.

igainEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
igainEvent = addEvent 4099 4 Nothing . fixRange

-- | Calculate stereo panoram automation event.

panEvent :: ArrowXml a => AutoTrack -> a XmlTree XmlTree
panEvent = addEvent 4201 6 (Just "Panner")

-- | Calculate event

iEvent :: ArrowXml a => String -> Natural -> AutoTrack -> a XmlTree XmlTree
iEvent dn i = addEvent (4201 + i) 4 (Just dn)

-- | Generate automation event for “send” track.

sendEvent :: ArrowXml a => String -> Natural -> AutoTrack -> a XmlTree XmlTree
sendEvent dn i = addEvent (4096 + i) 5 (Just dn) . bool id fixRange (i == 1)

-- | Most general way to create automation event. By event we (after Cubase)
-- mean the element representing whole automation track. This may be
-- confusing, so I explicitly mention it here.

addEvent :: ArrowXml a
  => Natural -- ^ Tags (magic number identifying what this thing will control)
  -> Natural -- ^ Flags (another magic number, some settings)
  -> Maybe String -- ^
  -> AutoTrack         -- ^ Automation track (values + delta times)
  -> a XmlTree XmlTree
addEvent tag flags dn t =
  mkObj (Just "MAutomationTrackEvent") Nothing Nothing
    [ mkInt   "Flags"  32
    , mkFloat "Start"  0
    , mkFloat "Length" (durFactor * totalDur t)
    , mkObj (Just "MAutoListNode") (Just "Node") Nothing
      [ mkMember "Domain"
        [ mkInt "Type" 0 ]
      , mkList "Events" "obj" (genEvents t) ]
    , mkAutoTrack dn
    , unlessNull (mkInt "Height" 42)
    , mkInt "Tag" tag
    , unlessNull (mkInt "TrackFlags" flags) ]
  where unlessNull a = if nullTrack t then none else a

-- | Generate automation track element with specified device name.

mkAutoTrack :: ArrowXml a => Maybe String -> a XmlTree XmlTree
mkAutoTrack dn = ifA (deep $ isAT dn) mkLink mkCmpl
  where mkCmpl = mkObj (Just "MAutomationTrack") name (Just $ genID dn)
          [ mkInt "Connection Type" (maybe 2 (const 7) dn)
          , mnone (mkString "Device Name") dn
          , mkInt "Read" 1
          , mkInt "Write" 0 ]
        mkLink = mkObj Nothing name (Just (genID dn)) []
        name   = Just "Track Device"

-- | Produce array of elements corresponding to points in our automation
-- track.

genEvents :: ArrowXml a => AutoTrack -> [a XmlTree XmlTree]
genEvents AutoTrack {..} = zipWith f atVal (fixDur atDur)
  where f v d = mkObj (Just "MParamEvent") Nothing Nothing
                [mkFloat "Start" d, mkFloat "Value" v]

-- | Fix range of volume\/gain values, so 1 corresponds to 100% volume.

fixRange :: AutoTrack -> AutoTrack
fixRange AutoTrack {..} = AutoTrack ((* 0.78908658027648926) <$> atVal) atDur

-- | Fix duration of every point in automation track. Apart from trivial
-- scaling of values we also transform delta times into start times.

fixDur :: [Double] -> [Double]
fixDur = fmap (* durFactor) . reverse . tail . foldl' f []
  where f []       a = [a, 0]
        f xs@(x:_) a = x + a : xs

-- | Generate device name for insertion slot given collection of insertion
-- slot GUIDs and index of this insertion slot.

genInsertDN
  :: [String]          -- ^ Insert slot GUIDs
  -> Natural           -- ^ Index of this insert slot
  -> Maybe String      -- ^ Device name
genInsertDN is n = wrap <$> is !!! n
  where wrap x = concat ["Inserts\\Slot", idx, "\\", x, "-0"]
        idx    = if n > 0 then " " ++ show (succ n) else ""

-- | Generate device name for send slot given its index.

genSendDN :: Natural -> String
genSendDN n = "Sends\\Slot" ++ if n > 0 then " " ++ show (succ n) else ""

-- | Generate device name for synth track given collection of synth GUIDs.

genSynthDN :: [String] -> Maybe String
genSynthDN ps = wrap <$> ps !!! 0
  where wrap x = "Slot\\" ++ x ++ "-0"

-- | Generate identifier from device name. Algorithm is not very original,
-- but it works.

genID :: Maybe String -> String
genID = show . (+ 13) . maybe 0 (sum . zipWith (+) [255,510..] . fmap ord)

----------------------------------------------------------------------------
-- Element Creation

-- | Generate object.

mkObj :: ArrowXml a
  => Maybe String      -- ^ Class name
  -> Maybe String      -- ^ Name attribute
  -> Maybe String      -- ^ Identifier
  -> [a XmlTree XmlTree] -- ^ Attributes
  -> a XmlTree XmlTree
mkObj c n i = mkelem "obj"
  [ mnone (sattr "class") c
  , mnone (sattr "name")  n
  , sattr "ID" (fromMaybe "7" i) ]

-- | Generate “member”.

mkMember :: ArrowXml a
  => String            -- ^ Name attribute
  -> [a XmlTree XmlTree] -- ^ Attributes
  -> a XmlTree XmlTree
mkMember name = mkelem "member" [sattr "name" name]

-- | Generate list.

mkList :: ArrowXml a
  => String            -- ^ Name attribute
  -> String            -- ^ Type attribute
  -> [a XmlTree XmlTree] -- ^ Attributes
  -> a XmlTree XmlTree
mkList name t = mkelem "list" [sattr "name" name, sattr "type" t]

-- | Generate string.

mkString :: ArrowXml a
  => String            -- ^ Name attribute
  -> String            -- ^ Value attribute
  -> a XmlTree XmlTree
mkString n v = mkNamedVal "string" n v += sattr "wide" "true"

-- | Generate integer (for our purposes it's always non-negative).

mkInt :: ArrowXml a
  => String            -- ^ Name attribute
  -> Natural           -- ^ Value attribute
  -> a XmlTree XmlTree
mkInt n v = mkNamedVal "int" n (show v)

-- | Generate floating point value.

mkFloat :: ArrowXml a
  => String            -- ^ Name attribute
  -> Double            -- ^ Value attribute
  -> a XmlTree XmlTree
mkFloat n v = mkNamedVal "float" n (show v)

-- | General way to generate named values (i.e. with “name” and “value”
-- attributes).

mkNamedVal :: ArrowXml a
  => String            -- ^ Type (tag name)
  -> String            -- ^ Name attribute
  -> String            -- ^ Value attribute
  -> a XmlTree XmlTree
mkNamedVal t n v = mkelem t [sattr "name" n, sattr "value" v] []

----------------------------------------------------------------------------
-- Get/Set/Access Information

-- | Get name of track(s).

trackName :: ArrowXml a => a XmlTree String
trackName = isAnyClass trackEvents /> isClass "MListNode" /> getStr "Name"

-- | Get collection of GUIDs for insertion slots.

insertGuids :: ArrowXml a => a XmlTree [String]
insertGuids = guidsIn $ isMember "InsertFolder" /> isList "Slot" /> isItem

-- | Get collection of GUIDS for synth slots.

synthGuids :: ArrowXml a => a XmlTree [String]
synthGuids = guidsIn $ isMember "Synth Slot"

-- | General utility for GUID extraction.

guidsIn :: ArrowXml a
  => a XmlTree XmlTree -- ^ Varying intermediate produce
  -> a XmlTree [String] -- ^ Collection of GUIDs
guidsIn a =
  listA $ getChildren         >>>
  isAnyClass tracks           />
  isMember "DeviceAttributes" />
  a                           />
  isMember "Plugin"           />
  isMember "Plugin UID"       />
  getStr   "GUID"

-- | Get string

getStr :: ArrowXml a => String -> a XmlTree String
getStr name =
  isElem >>> hasName "string" >>> hasAttrValue "name" (== name) >>>
  getAttrValue "value"

setInt :: ArrowXml a => String -> Int -> a XmlTree XmlTree
setInt name val  = processChildren $ setVal `when` rightInt
  where setVal   = addAttr "value" (show val)
        rightInt = isElem >>> hasName "int" >>> hasAttrValue "name" (== name)

----------------------------------------------------------------------------
-- Special Primitives

-- | Filter automation tracks corresponding

isAT :: ArrowXml a => Maybe String -> a XmlTree XmlTree
isAT i = isClass "MAutomationTrack" >>> hasAttrValue "ID" (== genID i)

-- | Perform some actions on elements with specified class name.

inList :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
inList name action = processChildren $ action `when` isList name

-- | Filter lists with given name.

isList :: ArrowXml a => String -> a XmlTree XmlTree
isList name = isElem >>> hasName "list" >>> hasAttrValue "name" (== name)

-- | Filter elements which are of one of given classes.

isAnyClass :: ArrowXml a
  => [String]          -- ^ Collection of class names
  -> a XmlTree XmlTree -- ^ Arrow that selects elements of those classes
isAnyClass cs = catA (isClass <$> cs)

-- | Perform some actions on elements with specified class name.

inClass :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
inClass cls action = processChildren $ action `when` isClass cls

-- | Filter elements with specified class name.

isClass :: ArrowXml a => String -> a XmlTree XmlTree
isClass cls = isElem >>> hasName "obj" >>> hasAttrValue "class" (== cls)

-- | Filter members with given names.

isMember :: ArrowXml a => String -> a XmlTree XmlTree
isMember name = isElem >>> hasName "member" >>> hasAttrValue "name" (== name)

-- | Filter items.

isItem :: ArrowXml a => a XmlTree XmlTree
isItem = isElem >>> hasName "item"

-- | This takes arrow to use when input is 'Just' value, otherwise it acts
-- as identity arrow.

mthis :: ArrowXml a => (b -> a XmlTree XmlTree) -> Maybe b -> a XmlTree XmlTree
mthis = maybe this

-- | If input is 'Nothing', XML stream is purged, otherwise given function
-- is applied.

mnone :: ArrowXml a => (b -> a XmlTree XmlTree) -> Maybe b -> a XmlTree XmlTree
mnone = maybe none

-- | Add elements\/attributes. Input of the second argument is current XML
-- (sub)tree and its result is added to it.

infixl 7 ++=

(++=) :: ArrowXml a
  => a XmlTree XmlTree -- ^ The first arrow
  -> a XmlTree XmlTree -- ^ Arrow generating new elements from the first arrow
  -> a XmlTree XmlTree -- ^ Arrow combining new elements and existing ones
a ++= b = a += (a >>> b)

-- | Perform given action on children that have specified name.

infixr 5 />/

(/>/) :: ArrowXml a
  => String            -- ^ Name of element
  -> a XmlTree XmlTree -- ^ Action to perform
  -> a XmlTree XmlTree -- ^ Resulting arrow
name />/ action = processChildren $ action `when` (isElem >>> hasName name)

-- | Safe list indexation.

infixl 9 !!!

(!!!) :: [a] -> Natural -> Maybe a
xs !!! n = listToMaybe $ drop (fromIntegral n) xs

----------------------------------------------------------------------------
-- Constants

-- | XML elements that cannot be self-closing.

nonEmptyElts :: [String]
nonEmptyElts = ["bin","member","obj","list"]

-- | Names of track events.

trackEvents :: [String]
trackEvents = ["MAudioTrackEvent","MInstrumentTrackEvent","MDeviceTrackEvent"]

-- | Names of tracks.

tracks :: [String]
tracks = ["MAudioTrack","MInstrumentTrack","MTrack"]

-- | For some reason Cubase likes to have empty automation track for volume
-- in all cases.

ritualVolumeEvent :: Map AutoType AutoTrack
ritualVolumeEvent = M.singleton Volume (AutoTrack [] [])

-- | Multiplier to convert number of whole notes into units understood by
-- Cubase. One quarter note is 480 units, so the whole note is 1920.

durFactor :: Double
durFactor = 1920
