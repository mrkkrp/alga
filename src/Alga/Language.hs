--
-- This is entry point of Alga.Lang library.
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

module Alga.Language
  ( SyntaxTree
  , Sel (..)
  , Statement (..)
  , Principle
  , NRatio
  , Element (..)
  , AlgaEnv
  , HasEnv (..)
  , runAlgaEnv
  , addDef
  , remDef
  , clearDefs
  , getPrin
  , getSrc
  , fullSrc
  , getRefs
  , purgeEnv
  , checkRecur
  , evalDef
  , eval
  , toPrin )
where

import Alga.Language.SyntaxTree (SyntaxTree, Sel (..), Statement (..))
import Alga.Language.Element (Principle, NRatio, Element (..))
import Alga.Language.Environment
import Alga.Language.Eval (evalDef, eval, toPrin)
