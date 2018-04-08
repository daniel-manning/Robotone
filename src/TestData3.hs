module TestData3 (
    problems, library, printingData
) where

import Prelude hiding ((/))

import Control.Arrow
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)

import Types
import Expansion
import TexBase
import Parser
import Tex
import RobotM
import Library
import Writeup
import Printing

--A first attempt in number theory

evenNumber = Problem "The number \\sqrt{2} is irrational" [] "irrational(sqrt(2))"


problems = [evenNumber]

expansionTableSource :: [(String, String)]
expansionTableSource = [
    ("irrational(a)", "¬rational(a)"),
    ("rational(k)", "exists a b.(equals(divide(a,b),k))"),
    ("even(a)", "exists k.(equals(times(2,k), a)")
  ]


expansionTable :: [(FormulaWithSlots, Formula)]
expansionTable = [(f / allVariablesInFormula f, f') |
                  (f, f') <- (parse formula *** parse formula) <$> expansionTableSource]

--NB: the term rewriting code does not use renameFormula  -- so DO NOT ever put quantifiers on RHS
--     of the rewrite table.
--  (This is only relevant if we introduce sets, etc., so that formulae can be inside terms.)

rewriteTableSource = [
    ("applyfn(compose(f,g),x)", "applyfn(f,applyfn(g,x))"),
    ("kthterm(applyfnpointwise(f,an),n)", "applyfn(f,kthterm(an,n))")
  ]


rewriteTable :: [(Term, [Variable], Term)]
rewriteTable = [(t, allVariablesInTerm t, t') |
                  (t, t') <- (parse term *** parse term) <$> rewriteTableSource ]

----------------------------------------------------------------------------------------------------


termPatterns' :: Map String Pattern
termPatterns' = Map.fromList [
  ]

formulaPatterns' :: Map String Pattern
formulaPatterns' = Map.fromList [
  ("sqrt", "$\\sqrt{%}$"),
  ("divide","$\\frac{%}{%}$"),
  ("equals", "$% = %$")
  ]

nounPatterns' :: Map String Pattern
nounPatterns' = Map.fromList [
  ]

adjectivePatterns' :: Map String Pattern
adjectivePatterns' = Map.fromList [
  ]

printingData = PrintingData termPatterns' formulaPatterns' nounPatterns' adjectivePatterns'

library = Library [
 ][
 ]
 expansionTable
 rewriteTable