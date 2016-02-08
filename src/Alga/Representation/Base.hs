--
-- Textual representation of basic elements in ALGA language.
--
-- Copyright © 2015–2016 Mark Karpov
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

module Alga.Representation.Base
  ( extremumAlias
  , panAlias
  , commentLine
  , productOp
  , divisionOp
  , sumOp
  , diffOp
  , loopOp
  , rotationOp
  , reverseOp
  , rangeOp
  , defOp
  , autoDel )
where

extremumAlias :: (String, String)
extremumAlias = ("off", "on")

panAlias :: (String, String, String)
panAlias = ("L", "C", "R")

commentLine :: String
commentLine = "#"

productOp :: String
productOp = "*"

divisionOp :: String
divisionOp = "/"

sumOp :: String
sumOp = "+"

diffOp :: String
diffOp = "-"

loopOp :: String
loopOp = "$"

rotationOp :: String
rotationOp = "^"

reverseOp :: String
reverseOp = "@"

rangeOp :: String
rangeOp = ".."

defOp :: String
defOp = "="

autoDel :: Char
autoDel = '.'
