-- -*- Mode: Haskell; -*-
--
-- Environment is formed via evaluation of definitions. This module
-- describes minimal ALGA environment in form of monad transformer.
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
{-# LANGUAGE TupleSections              #-}

module Alga.Language.Environment
    ( AlgaEnv (..)
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
    , setRandGen
    , newRandGen )
where

import Control.Arrow ((***), (>>>))
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T

import System.Random.Mersenne.Pure64

import Alga.Language.SyntaxTree
import Alga.Representation.Base (extremumAlias, panAlias)
import Alga.Representation.Show (showDefinition)

data AlgaEnvSt = AlgaEnvSt
    { stDefs    :: Defs
    , stRandGen :: PureMT }

type Defs = M.Map String SyntaxTree

newtype AlgaEnv m a = AlgaEnv
    { unAlgaEnv :: StateT AlgaEnvSt m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState AlgaEnvSt
             , MonadTrans
             , MonadIO )

runAlgaEnv :: Monad m => AlgaEnv m a -> m a
runAlgaEnv e = evalStateT (unAlgaEnv e) AlgaEnvSt
               { stDefs    = defaultDefs
               , stRandGen = pureMT 0 }

defaultDefs :: Defs
defaultDefs = M.fromList [ (a, [Value 0])
                         , (b, [Value 1])
                         , (l, [Value 0])
                         , (c, [Value (1 % 2)])
                         , (r, [Value 1]) ]
    where (a, b)    = extremumAlias
          (l, c, r) = panAlias

getDefs :: Monad m => AlgaEnv m Defs
getDefs = gets stDefs

setDefs :: Monad m => Defs -> AlgaEnv m ()
setDefs x = modify $ \e -> e { stDefs = x }

addDef :: Monad m => String -> SyntaxTree -> AlgaEnv m ()
addDef name tree = M.insert name tree <$> getDefs >>= setDefs

remDef :: Monad m => String -> AlgaEnv m ()
remDef name = M.delete name <$> getDefs >>= setDefs

clearDefs :: Monad m => AlgaEnv m ()
clearDefs = setDefs defaultDefs

getPrin :: Monad m => String -> AlgaEnv m SyntaxTree
getPrin name = (fromMaybe [] . M.lookup name) <$> getDefs

getSrc :: Monad m => String -> AlgaEnv m T.Text
getSrc name = showDefinition name <$> getPrin name

fullSrc :: Monad m => AlgaEnv m T.Text
fullSrc = (M.foldMapWithKey showDefinition . (M.\\ defaultDefs)) <$> getDefs

getRefs :: Monad m => AlgaEnv m [String]
getRefs = M.keys <$> getDefs

tDefs :: String -> Defs -> [String]
tDefs name defs = maybe mzero cm $ name `M.lookup` defs
    where cm               = (>>= f)
          f (Value      _) = mempty
          f (Section    x) = cm x
          f (Multi      x) = cm x
          f (CMulti     x) = x >>= (cm *** cm >>> uncurry (<>))
          f (Reference  x) = return x <> tDefs x defs
          f (Range    _ _) = mempty
          f (Product  x y) = f x <> f y
          f (Division x y) = f x <> f y
          f (Sum      x y) = f x <> f y
          f (Diff     x y) = f x <> f y
          f (Loop     x y) = f x <> f y
          f (Rotation x y) = f x <> f y
          f (Reverse    x) = f x

purgeEnv :: Monad m => [String] -> AlgaEnv m ()
purgeEnv tops = f <$> getDefs >>= setDefs
    where f defs = M.intersection defs $ M.unions [ts, ms defs, defaultDefs]
          ms     = M.unions . fmap toDefs . zipWith tDefs tops . repeat
          ts     = toDefs tops

checkRecur :: Monad m => String -> SyntaxTree -> AlgaEnv m Bool
checkRecur name tree = check <$> getDefs
    where check = elem name . tDefs name . M.insert name tree

setRandGen :: Monad m => Int -> AlgaEnv m ()
setRandGen x = modify $ \e -> e { stRandGen = pureMT (fromIntegral x) }

newRandGen :: Monad m => AlgaEnv m PureMT
newRandGen = do
  (n, g) <- randomWord64 <$> gets stRandGen
  modify $ \e -> e { stRandGen = pureMT n }
  return . pureMT . fst . randomWord64 $ g

toDefs :: [String] -> Defs
toDefs = M.fromList . fmap (, [])
