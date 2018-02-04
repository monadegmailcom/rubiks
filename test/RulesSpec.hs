module RulesSpec where

import Simulator.Rules
import Simulator.Types

import Test.Hspec

spec :: Spec
spec = describe "Check rules" $
    it "applies a permutation of all rules correctly" $ do
       let permutation = [1, 5, 10, 3, 0, 2, 9, 4, 11, 8, 6, 7]
           rules' = map (rules !!) permutation
           cube' = foldl (\c r -> map (applyRule r) c) solution rules'
           out =
              [ ([Blue,White,Red], [(Blue,Blue),(White,White),(Red,Red)])
              , ([Green,White,Orange], [(Orange,Blue),(White,White),(Green,Orange)])
              , ([Blue,Yellow,Red], [(Blue,Blue),(Yellow,Yellow),(Red,Red)])
              , ([Blue,White,Orange], [(White,Blue),(Blue,Yellow),(Orange,Orange)])
              , ([Green,Yellow,Red], [(Green,Green),(Red,White),(Yellow,Red)])
              , ([Green,Yellow,Orange], [(Green,Green),(Orange,White),(Yellow,Orange)])
              , ([Green,White,Red], [(White,Green),(Green,Yellow),(Red,Red)])
              , ([Blue,Yellow,Orange], [(Blue,Green),(Orange,Yellow),(Yellow,Orange)])
              , ([Blue,Yellow], [(Blue,Blue),(Yellow,Yellow)])
              , ([Blue,Orange], [(Blue,Blue),(Orange,Orange)])
              , ([Green,Orange], [(Green,White),(Orange,Green)])
              , ([Green,White], [(White,White),(Green,Orange)])
              , ([Blue,White], [(Blue,Red),(White,Green)])
              , ([White,Orange], [(White,Red),(Orange,Yellow)])
              , ([Green,Orange], [(Orange,Green),(Green,White)])
              , ([Blue,White], [(White,Green),(Blue,Red)])
              , ([Blue,Yellow], [(Yellow,Yellow),(Blue,Blue)])
              , ([White,Orange], [(Orange,Yellow),(White,Red)])
              , ([Blue,Orange], [(Orange,Orange),(Blue,Blue)])
              , ([Green,White], [(Green,Orange),(White,White)])
              ]
           toBrick ls ps = Brick (foldl1 addVV (map colorToVector ls))
                                 (map (\(lhs,rhs) -> (colorToVector lhs, rhs)) ps) 
           
           cube'' = map (uncurry toBrick) out
       cube' `shouldBe` cube''
