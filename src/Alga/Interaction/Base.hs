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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS  -fno-warn-orphans          #-}

module Alga.Interaction.Base
    ( AlgaIO
    , AlgaInt
    , runAlgaInt
    , AlgaSt (..)
    , AlgaCfg (..)
    , lift
    , liftEnv
    , getPrevLen
    , setPrevLen
    , getSrcFile
    , setSrcFile
    , getProg
    , setProg
    , getTempo
    , setTempo
    , getPrompt
    , getVerbose
    , getPrvCmd
    , getProgOp
    , getTempoOp
    , dfltSeed
    , dfltQuarter
    , dfltBeats
    , processDef )
where

import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Text.Format as F
import qualified System.Console.Haskeline as L

import Alga.Language

type AlgaIO = AlgaInt IO

newtype AlgaInt m a = AlgaInt
    { unAlgaInt :: StateT AlgaSt (ReaderT AlgaCfg (AlgaEnv m)) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState AlgaSt
             , MonadReader AlgaCfg
             , MonadIO )

instance MonadTrans AlgaInt where
    lift = AlgaInt . lift . lift . lift

liftEnv :: (Monad m) => AlgaEnv m a -> AlgaInt m a
liftEnv = AlgaInt . lift . lift

deriving instance L.MonadException m => L.MonadException (AlgaEnv m)
deriving instance L.MonadException m => L.MonadException (AlgaInt m)

data AlgaSt = AlgaSt
    { stPrevLen :: Int
    , stSrcFile :: String
    , stProg    :: Int
    , stTempo   :: Int }

data AlgaCfg = AlgaCfg
    { cfgPrompt  :: String
    , cfgVerbose :: Bool
    , cfgPrvCmd  :: String
    , cfgProgOp  :: String
    , cfgTempoOp :: String }

runAlgaInt :: Monad m => AlgaInt m a -> AlgaSt -> AlgaCfg -> m a
runAlgaInt m st cfg =
    runAlgaEnv (runReaderT (evalStateT (unAlgaInt m) st) cfg)

getPrevLen :: AlgaIO Int
getPrevLen = stPrevLen <$> get

setPrevLen :: Int -> AlgaIO ()
setPrevLen x = modify $ \e -> e { stPrevLen = x }

getSrcFile :: AlgaIO String
getSrcFile = stSrcFile <$> get

setSrcFile :: String -> AlgaIO ()
setSrcFile x = modify $ \e -> e { stSrcFile = x }

getProg :: AlgaIO Int
getProg = stProg <$> get

setProg :: Int -> AlgaIO ()
setProg x = modify $ \e -> e { stProg = x }

getTempo :: AlgaIO Int
getTempo = stTempo <$> get

setTempo :: Int -> AlgaIO ()
setTempo x = modify $ \e -> e { stTempo = x }

getPrompt :: AlgaIO String
getPrompt = cfgPrompt <$> ask

getVerbose :: AlgaIO Bool
getVerbose = cfgVerbose <$> ask

getPrvCmd :: AlgaIO String
getPrvCmd = cfgPrvCmd <$> ask

getProgOp :: AlgaIO String
getProgOp = cfgProgOp <$> ask

getTempoOp :: AlgaIO String
getTempoOp = cfgTempoOp <$> ask

dfltSeed :: Int
dfltSeed = 0

dfltQuarter :: Int
dfltQuarter = 24

dfltBeats :: Int
dfltBeats = 16

processDef :: String -> SyntaxTree -> AlgaIO ()
processDef n t = do
  recursive <- liftEnv $ checkRecur n t
  if recursive
  then liftIO $ F.print "Rejected recursive definition for «{}».\n" (F.Only n)
  else liftEnv (addDef n t) >> liftIO (F.print "• «{}»\n" (F.Only n))
