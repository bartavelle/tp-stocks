module Main where

import Basics as B
import qualified Inventory as I
import InventorySpec (checkoutTests)
import qualified S2021.SampleStudent as S
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe, xdescribe)
import Test.Hspec.QuickCheck (prop)

basics :: B.Solution -> SpecWith ()
basics sol = describe "basics" $ do
  describe "simple operations" $ do
    prop "add" (\a b -> _add sol a b == a + b)
    prop "divide" (\a b -> b == 0 || _divide sol a b == a `div` b)
    prop "divide' /= 0" (\a b -> b == 0 || _divide' sol a b == IntSuccess (div a b))
    prop "divide' == 0" (\a -> _divide' sol a 0 == IntFailure)
    prop "divide'' /= 0" (\a b -> b == 0 || _divide'' sol a b == B.Success (div a b))
    prop "divide'' == 0" (\a -> _divide'' sol a 0 == B.Failure)
  describe "eval" $ do
    prop "Value" (\a -> _eval sol (Value a) == a)
    prop "Add" (\a b -> _eval sol (Add (Value a) (Value b)) == a + b)
    prop "Sub" (\a b -> _eval sol (Sub (Value a) (Value b)) == a - b)
    prop "Mul" (\a b -> _eval sol (Mul (Value a) (Value b)) == a * b)
    it "test 1" (_eval sol (Add (Mul (Value 5) (Value 3)) (Value 8)) `shouldBe` 5 * 3 + 8)
  describe "eval'" $ do
    prop "Value" (\a -> _eval' sol (Value a) == B.Success a)
    prop "Add" (\a b -> _eval' sol (Add (Value a) (Value b)) == B.Success (a + b))
    prop "Sub" (\a b -> _eval' sol (Sub (Value a) (Value b)) == B.Success (a - b))
    prop "Mul" (\a b -> _eval' sol (Mul (Value a) (Value b)) == B.Success (a * b))
    it "test 1" (_eval' sol (Add (Mul (Value 5) (Value 3)) (Value 8)) `shouldBe` B.Success (5 * 3 + 8))
    it "test 2" (_eval' sol (Div (Div (Value 5) (Value 3)) (Value 0)) `shouldBe` B.Failure)
    it "test 3" (_eval' sol (Div (Div (Value 5) (Value 0)) (Value 1)) `shouldBe` B.Failure)
  describe "lists" $ do
    let foo :: Int -> Maybe Char
        foo x
          | x < 0 = Just 'a'
          | x > 0 = Just 'b'
          | otherwise = Nothing
    it "listHead1" (_listHead sol (Cons 1 (Cons 2 Empty)) `shouldBe` Success (1 :: Int))
    it "listHead2" (_listHead sol (Empty :: List Int) `shouldBe` Failure)
    prop "foo" (foo 3 == Just 'b')
    prop "listSum" (\l -> _listSum sol (fromList l) == sum l)
    prop "listEq" (\l1 l2 -> _listEq sol (fromList l1) (fromList l2) == ((l1 :: [Int]) == l2))
    prop "toList" (\l -> _toList sol (fromList l) == (l :: [Int]))
    prop "lmap" (\l -> _toList sol (_lmap sol foo (fromList l)) == fmap foo l)

    xdescribe "pending tests (edit test if you wrote the ltraverse function)" $
      prop "ltraverse" (\l -> fmap (_toList sol) (_ltraverse sol foo (fromList l)) == traverse foo (l :: [Int]))

fromList :: [a] -> List a
fromList = foldr Cons Empty

inventoryspecs :: I.Solution -> SpecWith ()
inventoryspecs sol = describe "Inventory" $ do
  checkoutTests sol

main :: IO ()
main = hspec $ do
  basics S.bsolution
  inventoryspecs S.isolution
