--
-- This module defines abstract syntax tree of ALGA language.
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

module Alga.Language.SyntaxTree
  ( SyntaxTree
  , Sel (..) )
where

import Alga.Language.Element (NRatio)

-- | Syntax tree in our case is just a collection of syntactic elements.

type SyntaxTree = [Sel]

-- | Syntactic element corresponds to language features. Some of them have
-- direct corresponding constructor in 'Alga.Language.Element.Element',
-- others have to be simplified first.

data Sel
  = Value     NRatio   -- ^ Literal value
  | Section   [Sel]    -- ^ Section
  | Multi     [Sel]    -- ^ Multivalue
  | CMulti    [([Sel], [Sel])] -- ^ Conditional multivalue
  | Reference String   -- ^ Reference (name of variable)
  | Range     NRatio NRatio -- ^ Range of values
  | Product   Sel Sel  -- ^ Product of principles
  | Division  Sel Sel  -- ^ Division of principles
  | Sum       Sel Sel  -- ^ Sum of principles
  | Diff      Sel Sel  -- ^ Subtraction of principles
  | Loop      Sel Sel  -- ^ Loop
  | Rotation  Sel Sel  -- ^ Rotation
  | Reverse   Sel      -- ^ Reversed principle
    deriving (Eq, Show)
