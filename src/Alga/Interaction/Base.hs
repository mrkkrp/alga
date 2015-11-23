-- -*- Mode: Haskell; -*-
--
-- This module describes monad for interactive REPL and some basic
-- functions.
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

module Alga.Interaction.Base
  ( AlgaSt  (..)
  , AlgaCfg (..)
  , Alga
  , runAlga
  , defaultSeed
  , defaultBeats
  , processDef
  , showRatio )
where

import Alga.Language
import Alga.Representation
import Alga.Translation
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Ratio (numerator, denominator, Ratio)
import Formatting
import Numeric.Natural
import Path

-- | ALGA REPL state.

data AlgaSt = AlgaSt
  { stBackend :: AlgaBackend -- ^ Selected backend (DAW dependent)
  , stPrevLen :: Natural -- ^ Length of preview principles
  , stSrcFile :: Path Abs File -- ^ Name of current source file
  }

-- | ALGA REPL configuration.

data AlgaCfg = AlgaCfg
  { cfgPrecision :: Double -- ^ Precision for some REPL utilities
  , cfgPrompt    :: String -- ^ REPL prompt
  , cfgVerbose   :: Bool -- ^ Verbose mode?
  }

-- | A synonym for MIDA monad stack.

type Alga = StateT AlgaSt (ReaderT AlgaCfg (AlgaEnv IO))

-- | Run ALGA monad stack.

runAlga
  :: Alga a            -- ^ ALGA monad stack
  -> AlgaSt            -- ^ Initial state
  -> AlgaCfg           -- ^ Configuration
  -> IO a
runAlga m st cfg = runAlgaEnv (runReaderT (evalStateT m st) cfg)

-- | Default seed for random number generator.

defaultSeed :: Natural
defaultSeed = 0

-- | Default duration of automation to calculate in whole notes, note that
-- this is actually floating point value.

defaultBeats :: Double
defaultBeats = 4

-- | Process a principle definition.

processDef :: (HasEnv m, MonadIO m)
  => String            -- ^ Definition name
  -> SyntaxTree        -- ^ AST for that definition
  -> m ()
processDef n t = do
  recursive <- checkRecur n t
  if recursive
  then liftIO $
    fprint ("Rejected recursive definition for «" % string % "».\n") n
  else do addDef n t
          liftIO $ fprint ("• «" % string % "»\n") n

-- | Render a ratio.

showRatio :: (Integral a, Show a) => Ratio a -> String
showRatio x =
  let n = numerator   x
      d = denominator x
  in if d == 1
     then show n
     else show n ++ divisionOp ++ show d
