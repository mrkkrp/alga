--
-- Tests suite of ALGA.
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

{-# OPTIONS -fno-warn-orphans #-}

module Main (main) where

import Alga.Representation
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "probeAlga" $
    it "returns True for every correct Statement" $
      property (probeAlga . showStatement)
  describe "parser and printer" $
    it "printed representation of statement can be parsed back" $
      property $ \stmt ->
        parseAlga "" (showStatement stmt) `shouldBe` Right [stmt]
