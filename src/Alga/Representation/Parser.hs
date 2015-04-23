-- -*- Mode: Haskell; -*-
--
-- This module describes how to build syntax tree from textual
-- representation of ALGA statements.
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

{-# LANGUAGE OverloadedStrings #-}

module Alga.Representation.Parser
    ( Statement (..)
    , probeAlga
    , parseAlga )
where

import Control.Monad (void)
import Data.Functor.Identity
import Data.List (nub)
import qualified Data.Text.Lazy as T

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Text.Lazy
import qualified Text.Parsec.Token as Token

import Alga.Language.SyntaxTree (SyntaxTree, Sel (..))
import qualified Alga.Representation.Base as B

data Statement
    = Definition String SyntaxTree
    | Exposition SyntaxTree
      deriving (Eq, Show)

probeAlga :: T.Text -> Bool
probeAlga txt = not $ or ["," `T.isSuffixOf` stripped
                         , f ("[", "]")
                         , f ("{", "}")
                         , f ("<", ">")
                         , f ("(", ")") ]
    where stripped = T.strip txt
          f (x, y) = (&&) <$> (> 0) <*> (/= g y) $ g x
          g x      = T.count x stripped

parseAlga :: String -> T.Text -> Either String [Statement]
parseAlga file txt =
    case parse parser file txt of
      Right x -> if null x
                 then Left $ '\"' : file ++ "\":\ninvalid definition syntax"
                 else Right x
      Left  x -> Left . show $ x
    where parser = if T.pack B.defOp `T.isInfixOf` txt
                   then pSource
                   else return <$> pExposition

pSource :: Parser [Statement]
pSource = whiteSpace *> many pDefinition <* eof

pDefinition :: Parser Statement
pDefinition = Definition <$> identifier <* reservedOp B.defOp <*> pPrinciple

pExposition :: Parser Statement
pExposition = Exposition <$> (whiteSpace *> pPrinciple <* eof)

pPrinciple :: Parser SyntaxTree
pPrinciple = sepBy (pExpression <|> pElement) (optional comma)

pElement :: Parser Sel
pElement
    =  try pRange
   <|> pValue
   <|> try pReference
   <|> pSection
   <|> try pMulti
   <|> pCMulti
   <?> "element"

pRange :: Parser Sel
pRange = Range <$> pFloat <* reservedOp B.rangeOp <*> pFloat

pValue :: Parser Sel
pValue = Value <$> pFloat

pFloat :: Parser Double
pFloat = try float <|> fromIntegral <$> natural <?> "literal value"

pReference :: Parser Sel
pReference = Reference <$> identifier <* notFollowedBy (reservedOp B.defOp)

pSection :: Parser Sel
pSection = Section <$> brackets pPrinciple

pMulti :: Parser Sel
pMulti = Multi <$> braces pPrinciple

pCMulti :: Parser Sel
pCMulti = CMulti <$> braces (many $ (,) <$> angles pPrinciple <*> pPrinciple)

pExpression :: Parser Sel
pExpression = buildExpressionParser optTable (parens pExpression <|> pElement)
              <?> "expression"

optTable :: [[Operator T.Text () Identity Sel]]
optTable =
    [[ Prefix (reservedOp B.reverseOp >> return Reverse ) ]
     , [ Infix (reservedOp B.productOp  >> return Product ) AssocLeft
       , Infix (reservedOp B.divisionOp >> return Division) AssocLeft
       , Infix (reservedOp B.sumOp      >> return Sum     ) AssocLeft
       , Infix (reservedOp B.diffOp     >> return Diff    ) AssocLeft
       , Infix (reservedOp B.loopOp     >> return Loop    ) AssocLeft
       , Infix (reservedOp B.rotationOp >> return Rotation) AssocLeft ]]

lang :: GenLanguageDef T.Text () Identity
lang = Token.LanguageDef
       { Token.commentStart    = ""
       , Token.commentEnd      = ""
       , Token.commentLine     = B.commentLine
       , Token.nestedComments  = True
       , Token.identStart      = letter   <|> char '_'
       , Token.identLetter     = alphaNum <|> char '_' <|> char B.autoDel
       , Token.opStart         = Token.opLetter lang
       , Token.opLetter        = oneOf . nub . concat $ langOps
       , Token.reservedOpNames = langOps
       , Token.reservedNames   = []
       , Token.caseSensitive   = True }

langOps :: [String]
langOps =
    [ B.productOp
    , B.divisionOp
    , B.sumOp
    , B.diffOp
    , B.loopOp
    , B.rotationOp
    , B.reverseOp
    , B.rangeOp
    , B.defOp ]

lexer :: Token.GenTokenParser T.Text () Identity
lexer = Token.makeTokenParser lang

angles :: Parser a -> Parser a
angles = Token.angles lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

comma :: Parser ()
comma = void $ Token.comma lexer

identifier :: Parser String
identifier = Token.identifier lexer

float :: Parser Double
float = Token.float lexer

natural :: Parser Integer
natural = Token.natural lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer