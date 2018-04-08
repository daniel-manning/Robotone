import Test.Hspec
import Test.QuickCheck
import Types
import Parser

formula1 = Exists [Variable "delta" 1 TPositiveRealNumber VTNormal (Dependencies [] [])] (And [AtomicFormula (Predicate "in") [VariableTerm (Variable "y" 1 TPoint VTNormal (Dependencies [] [])),ApplyFn (Function "intersect") [VariableTerm (Variable "A" 1 TSet VTNormal (Dependencies [] [])),VariableTerm (Variable "B" 1 TSet VTNormal (Dependencies [] []))]],AtomicFormula (Predicate "lessthan") [ApplyFn (Function "d") [VariableTerm (Variable "x" 1 TPoint VTNormal (Dependencies [] [])),VariableTerm (Variable "y" 1 TPoint VTNormal (Dependencies [] []))],VariableTerm (Variable "delta" 1 TPositiveRealNumber VTNormal (Dependencies [] []))]])

formula2 = UniversalImplies [Variable "x" 1 TPoint VTNormal (Dependencies [] [])] [AtomicFormula (Predicate "in") [VariableTerm (Variable "x" 1 TPoint VTNormal (Dependencies [] [])),ApplyFn (Function "intersect") [VariableTerm (Variable "A" 1 TSet VTNormal (Dependencies [] [])),VariableTerm (Variable "B" 1 TSet VTNormal (Dependencies [] []))]]] (Exists [Variable "delta" 1 TPositiveRealNumber VTNormal (Dependencies [] [])] (UniversalImplies [Variable "y" 1 TPoint VTNormal (Dependencies [] [])] [AtomicFormula (Predicate "lessthan") [ApplyFn (Function "d") [VariableTerm (Variable "x" 1 TPoint VTNormal (Dependencies [] [])),VariableTerm (Variable "y" 1 TPoint VTNormal (Dependencies [] []))],VariableTerm (Variable "delta" 1 TPositiveRealNumber VTNormal (Dependencies [] []))]] (AtomicFormula (Predicate "in") [VariableTerm (Variable "y" 1 TPoint VTNormal (Dependencies [] [])),ApplyFn (Function "intersect") [VariableTerm (Variable "A" 1 TSet VTNormal (Dependencies [] [])),VariableTerm (Variable "B" 1 TSet VTNormal (Dependencies [] []))]])))

formula3 = UniversalImplies [Variable "y" 1 TPoint VTNormal (Dependencies [] [])] [AtomicFormula (Predicate "lessthan") [ApplyFn (Function "d") [VariableTerm (Variable "x" 1 TPoint VTNormal (Dependencies [] [])),VariableTerm (Variable "y" 1 TPoint VTNormal (Dependencies [] []))],VariableTerm (Variable "delta" 1 TPositiveRealNumber VTNormal (Dependencies [] []))]] (AtomicFormula (Predicate "in") [VariableTerm (Variable "y" 1 TPoint VTNormal (Dependencies [] [])),ApplyFn (Function "intersect") [VariableTerm (Variable "A" 1 TSet VTNormal (Dependencies [] [])),VariableTerm (Variable "B" 1 TSet VTNormal (Dependencies [] []))]])

main :: IO ()
main = hspec $ do
  describe "Function parsing" $ do
    it "should parse function" $
      parse formula "exists delta.(in(y, intersect(A, B)) & lessthan(d(x, y), delta))" `shouldBe` formula1

    it "should parse function" $
      parse formula "forall x.(in(x, intersect(A, B)) => exists delta.(forall y.(lessthan(d(x, y), delta) => in(y, intersect(A, B)))))" `shouldBe` formula2

    it "should parse function" $
      parse formula "forall y.(lessthan(d(x, y), delta) => in(y, intersect(A, B)))" `shouldBe` formula3
