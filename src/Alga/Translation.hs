-- -*- Mode: Haskell; -*-
--
-- This module performs patching of data structure that represent tracks
-- according of state of ALGA environment.
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

module Alga.Translation
    ( topDefs
    , patchAuto
    , cubaseBackend
    , ardourBackend )
where

import Alga.Translation.Ardour
import Alga.Translation.Base
import Alga.Translation.Cubase
