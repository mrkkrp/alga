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
    , getBackend
    , setBackend
    , getPrevLen
    , setPrevLen
    , getSrcFile
    , setSrcFile
    , getPrecision
    , getPrompt
    , getVerbose
    , dfltSeed
    , dfltBeats
    , processDef
    , pRational
    , toBackend )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Ratio (numerator, denominator)

import qualified Data.Text.Format as F
import qualified System.Console.Haskeline as L

import Alga.Language
import Alga.Representation
import Alga.Translation

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
    { stBackend :: AlgaBackend
    , stPrevLen :: Int
    , stSrcFile :: String }

data AlgaCfg = AlgaCfg
    { cfgPrecision :: Double
    , cfgPrompt    :: String
    , cfgVerbose   :: Bool }

runAlgaInt :: Monad m => AlgaInt m a -> AlgaSt -> AlgaCfg -> m a
runAlgaInt m st cfg =
    runAlgaEnv (runReaderT (evalStateT (unAlgaInt m) st) cfg)

getBackend :: AlgaIO AlgaBackend
getBackend = stBackend <$> get

setBackend :: AlgaBackend -> AlgaIO ()
setBackend x = modify $ \e -> e { stBackend = x }

getPrevLen :: AlgaIO Int
getPrevLen = stPrevLen <$> get

setPrevLen :: Int -> AlgaIO ()
setPrevLen x = modify $ \e -> e { stPrevLen = x }

getSrcFile :: AlgaIO String
getSrcFile = stSrcFile <$> get

setSrcFile :: String -> AlgaIO ()
setSrcFile x = modify $ \e -> e { stSrcFile = x }

getPrecision :: AlgaIO Double
getPrecision = cfgPrecision <$> ask

getPrompt :: AlgaIO String
getPrompt = cfgPrompt <$> ask

getVerbose :: AlgaIO Bool
getVerbose = cfgVerbose <$> ask

dfltSeed :: Int
dfltSeed = 0

dfltBeats :: Double
dfltBeats = 4

processDef :: String -> SyntaxTree -> AlgaIO ()
processDef n t = do
  recursive <- liftEnv $ checkRecur n t
  if recursive
  then liftIO $ F.print "Rejected recursive definition for «{}».\n" (F.Only n)
  else liftEnv (addDef n t) >> liftIO (F.print "• «{}»\n" (F.Only n))

pRational :: Rational -> String
pRational x = if d == 1
              then show n
              else show n ++ divisionOp ++ show d
    where n = numerator   x
          d = denominator x

toBackend :: String -> AlgaBackend
toBackend "ardour" = ardourBackend
toBackend _        = cubaseBackend
