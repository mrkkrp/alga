--
-- This module describes how ALGA processes commands in interactive
-- mode. These commands are also used in batch mode.
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

module Alga.Interaction
  ( AlgaSt  (..)
  , AlgaCfg (..)
  , Alga
  , runAlga
  , cmdBackend
  , cmdLoad
  , cmdMake
  , interaction
  , defaultSeed
  , defaultBeats )
where

import Alga.Interaction.Base
import Alga.Interaction.Commands
import Alga.Language
import Alga.Representation
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text.Lazy (Text)
import Data.Version (showVersion)
import Formatting
import Path
import Paths_alga (version)
import System.IO
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified System.Console.Haskeline as L

-- | Entry point for REPL interaction.

interaction :: Alga ()
interaction = do
  liftIO $ hSetBuffering stdin LineBuffering
  liftIO $ fprint
    ("ALGA Interactive Environment " % string % "\n") (showVersion version)
  L.runInputT (L.setComplete completionFunc L.defaultSettings) algaRepl

-- | Infinite REPL loop inside Haskeline's 'InputT' monad transformer.

algaRepl :: L.InputT Alga ()
algaRepl = do
  input <- getMultiline ""
  case input of
    Just x  -> do
      if T.pack cmdPrefix `T.isPrefixOf` T.strip x
      then lift $ processCmd x
      else lift $ processExpr x
      algaRepl
    Nothing -> return ()

-- | Read multi-line.

getMultiline :: Text -> L.InputT Alga (Maybe Text)
getMultiline prv = do
  prompt <- lift (asks cfgPrompt)
  input  <- L.getInputLine $
            if T.null prv then prompt else replicate (length prompt) ' '
  case input of
    Just x -> let r = prv `T.append` T.pack x `T.append` "\n"
              in if probeAlga r
                 then return (Just r)
                 else getMultiline r
    Nothing -> return Nothing

-- | Process expression.

processExpr :: Text -> Alga ()
processExpr expr = do
  file <- gets stSrcFile
  case parseAlga (fromAbsFile file) expr of
    Right x -> mapM_ f x
    Left  x -> liftIO (putStr x)
    where f (Definition n t) = processDef n t
          f (Exposition   t) = do
            len     <- gets stPrevLen
            verbose <- asks cfgVerbose
            result  <- eval t
            prin    <- toPrin t
            liftIO . when verbose $
              fprint ("≡ " % text) (showPrinciple prin)
            spitList $ take (fromIntegral len) result

-- | Pretty-print stream of non-negative ratios.

spitList :: [NRatio] -> Alga ()
spitList [] = liftIO $ T.putStrLn "⇒ ⊥"
spitList xs = liftIO $ fprint ("⇒ " % string % "…\n") l
  where l = unwords $ showRatio <$> xs
