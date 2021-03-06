--
-- Here we describe how to build textual representation of syntax trees and
-- principles.
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

module Alga.Representation.Show
  ( showStatement
  , showDefinition
  , showSyntaxTree
  , showPrinciple )
where

import Alga.Language.Element
import Alga.Language.SyntaxTree
import Control.Arrow ((***), (>>>))
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Ratio (numerator, denominator)
import Data.Text.Lazy (Text)
import qualified Alga.Representation.Base   as B
import qualified Data.Text.Lazy.Builder     as T (Builder, fromString, toLazyText)
import qualified Data.Text.Lazy.Builder.Int as T (decimal)
import qualified Data.List.NonEmpty         as NE

-- | Render a statement. This handles definitions and expositions.

showStatement :: Statement -> Text
showStatement (Definition n t) = showDefinition n t
showStatement (Exposition   t) = showSyntaxTree t

-- | Render definition.

showDefinition
  :: String            -- ^ Reference name
  -> SyntaxTree        -- ^ Syntax tree
  -> Text              -- ^ Textual representation of definition
showDefinition n = T.toLazyText . showDefinition' n

-- | Render syntax tree.

showSyntaxTree :: SyntaxTree -> Text
showSyntaxTree = T.toLazyText . showSyntaxTree'

-- | Show principle. This is useful for printing of simplified principles
-- back to user. We can use the same pretty-printing algorithm as for syntax
-- trees, but this requires us to perform transformation from 'Principle' to
-- 'SyntaxTree', which is trivial.

showPrinciple :: Principle -> Text
showPrinciple = showSyntaxTree . toSyntaxTree

-- | This is used by 'showDefinition'. It just creates lazy text builder.

showDefinition'
  :: String            -- ^ Reference name
  -> SyntaxTree        -- ^ Syntax tree
  -> T.Builder         -- ^ Lazy text builder for this definition
showDefinition' n t = T.fromString n <> pad B.defOp <> showSyntaxTree' t

-- | Convert syntax tree into lazy text builder.

showSyntaxTree' :: SyntaxTree -> T.Builder
showSyntaxTree' t = cm f t <> "\n"
  where
    cm g xs           = mconcat . intersperse " " $ g <$> xs
    p x@(Value     _) = f x
    p x@(Section   _) = f x
    p x@(Multi     _) = f x
    p x@(CMulti    _) = f x
    p x@(Reference _) = f x
    p x@(Range   _ _) = f x
    p x               = "(" <> f x <> ")"
    f (Value       x) = pRational x
    f (Section     x) = "[" <> cm f x <> "]"
    f (Multi       x) = "{" <> cm f x <> "}"
    f (CMulti      x) = "{" <> cm (c *** cm f >>> uncurry (<>)) (NE.toList x) <> "}"
    f (Reference   x) = T.fromString x
    f (Range     x y) = pRational x <> T.fromString B.rangeOp <> pRational y
    f (Product   x y) = f x <> pad B.productOp   <> p y
    f (Division x@(Value _) y@(Value _)) =
        f x <> T.fromString B.divisionOp  <> p y
    f (Division  x y) = f x <> pad B.divisionOp  <> p y
    f (Sum       x y) = f x <> pad B.sumOp       <> p y
    f (Diff      x y) = f x <> pad B.diffOp      <> p y
    f (Loop      x y) = f x <> pad B.loopOp      <> p y
    f (Rotation  x y) = f x <> pad B.rotationOp  <> p y
    f (Reverse     x) = T.fromString B.reverseOp <> p x
    c xs              = "<" <> cm f xs <> "> "

-- | Convert principle to syntax tree to show it.

toSyntaxTree :: Principle -> SyntaxTree
toSyntaxTree = fmap f
  where f (Val  x) = Value x
        f (Sec  x) = Section $ f <$> x
        f (Mul  x) = Multi   $ f <$> x
        f (CMul x) = CMulti  $ (toSyntaxTree *** toSyntaxTree) <$> x

-- | Pad given string with single space on both sides.

pad :: String -> T.Builder
pad op = " " <> T.fromString op <> " "

-- | Render non-negative ratio.

pRational :: NRatio -> T.Builder
pRational x =
  let n = numerator   x
      d = denominator x
  in if d == 1
     then T.decimal n
     else T.decimal n <> T.fromString B.divisionOp <> T.decimal d
