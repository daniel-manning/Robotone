{-# LANGUAGE FlexibleInstances #-}

module WriteupBase where

import Types
import Printing

class Writeup a where
    writeup :: PrintingData -> a -> String

type Follows = Bool
instance Pretty Follows where
   pretty(a) = show a

data Clause = StubClause String
            | ProofDone                     -- we are done
            | TargetReminder [Statement]    -- we are trying to show X
            | TargetIs [Statement]          -- we would like to show X
            | TargetIsIE [Statement] [Statement]         -- we would like to show X, i.e. Y
            | Let [LetArgs]
            | Take [Variable]
            | Assertion [Statement]         -- X
            | WeMayTake [VariableChoice]            -- we may therefore take x = T
            | ThereforeSettingDone [VariableChoice] -- therefore, setting x = T, we are done
            | If [Statement] Statement --P and P' if Q
            | Whenever [Statement] Statement --P and P' whenever Q
            | Iff [Statement] [Statement]
            | AssumeNow [Statement]
            | ClearlyTheCaseDone  -- But this is clearly the case, so we are done

            | ExistVars [Variable] Clause
            | Since [Statement] Follows [Clause]
            | ByDefSince [Statement] Follows [Clause]
            | WeKnowThat [Clause]
            | But Clause  --may turn into an adverb

instance Pretty Clause where
    pretty (StubClause s) = "stubclause " ++ s
    pretty (ProofDone) = "proofdone"                     -- we are done
    pretty (TargetReminder s) = "targetreminder " ++ (show $ map pretty s)    -- we are trying to show X
    pretty (TargetIs s) = "targetis " ++ (show $ map pretty s)          -- we would like to show X
    pretty (TargetIsIE s st) = "targetisIE " ++ "[" ++ (show $ map pretty s) ++ "] ["  ++ (show $ map pretty st) ++ "]"       -- we would like to show X, i.e. Y
    pretty (Let l) = "Let " ++ (show $ map pretty l)
    pretty (Take v) = "take " ++ (show $ map pretty v)
    pretty (Assertion s) = "assertion " ++ (show $ map pretty s)     -- X
    pretty (WeMayTake v) = "wemaytake " ++ (show $ map pretty v)     -- we may therefore take x = T
    pretty (ThereforeSettingDone v) = "thereforesettingdone " ++ (show $ map pretty v) -- therefore, setting x = T, we are done
    pretty (If st s ) = "if " ++ (show $ map pretty st) ++ pretty(s) --P and P' if Q
    pretty (Whenever st s) = "whenever " ++ (show $ map pretty st) ++ pretty(s) --P and P' whenever Q
    pretty (Iff st s) = "iff " ++ (show $ map pretty st) ++ (show $ map pretty s)
    pretty (AssumeNow st) = "assumenow " ++ (show $ map pretty st)
    pretty (ClearlyTheCaseDone) = "clearlythecasedone"  -- But this is clearly the case, so we are done

    pretty (ExistVars v c) = "ExistVars " ++ (show $ map pretty v) ++ pretty(c)
    pretty (Since s f c) = "Since " ++ (show $ map pretty s) ++ pretty(f) ++ (show $ map pretty c)
    pretty (ByDefSince s f c) = "ByDefSince " ++ (show $ map pretty s) ++ pretty(f) ++ (show $ map pretty c)
    pretty (WeKnowThat c) = "WeKnowThat " ++ (show $ map pretty c)
    pretty (But c) = "But " ++ pretty(c)

data LetArgs = BeSuchThat [Variable] [Statement]  -- let x and y be be s.t X
             | Suspended [Variable]                 -- let x be a constant to be chosen later
--             | Unconstrained Variable             -- let epsilon > 0

instance Pretty LetArgs where
    pretty (BeSuchThat v s) = "BeSuchThat " ++ (show $ map pretty v) ++ (show $ map pretty v)
    pretty (Suspended v) = "Suspended " ++ (show $ map pretty v)

data Adverb = Then | So  | Also | AndA | IE | Therefore | Thus   deriving Eq

data Unit = Unit (Maybe Adverb) [Clause]