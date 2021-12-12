module InventorySpec (checkoutTests) where

import qualified Data.Map.Strict as M
import qualified Inventory as I
import Test.Hspec

checkoutTests :: I.Solution -> Spec
checkoutTests sol = do
  describe "getters" $ do
    let sampledesc = I.ItemDesc "name" 5 I.Book
    it "getName" $ I._getName sol sampledesc `shouldBe` "name"
    it "getPrice" $ I._getPrice sol sampledesc `shouldBe` 5
    it "getType" $ I._getType sol sampledesc `shouldBe` I.Book
  describe "getPrice" $ do
    it "known item 1" $ I._getItemPrice sol I.sampleCatalog 1 `shouldBe` Right 785
    it "known item 2" $ I._getItemPrice sol I.sampleCatalog 2 `shouldBe` Right 200
    it "known item 3" $ I._getItemPrice sol I.sampleCatalog 3 `shouldBe` Right 24700
    it "unknown item" $ I._getItemPrice sol I.sampleCatalog 10 `shouldBe` Left I.DescriptionNotFound
  describe "updateInventory" $ do
    it "empty inventory" $ I._updateInventory sol mempty (M.fromList [(1, 4)]) `shouldBe` Left I.OutOfStock
    it "empty basket" $ I._updateInventory sol (M.fromList [(1, 4)]) mempty `shouldBe` Right (M.fromList [(1, 4)])
    it "oos1" $ I._updateInventory sol (M.fromList [(1, 4), (2, 5)]) (M.fromList [(1, 5)]) `shouldBe` Left I.OutOfStock
    it "oos2" $ I._updateInventory sol (M.fromList [(1, 4), (2, 5)]) (M.fromList [(3, 1)]) `shouldBe` Left I.OutOfStock
    it "oos3" $ I._updateInventory sol (M.fromList [(1, 4), (2, 5)]) (M.fromList [(1, 5), (2, 6)]) `shouldBe` Left I.OutOfStock
    it "oos4" $ I._updateInventory sol (M.fromList [(1, 4), (2, 5)]) (M.fromList [(1, 3), (2, 6)]) `shouldBe` Left I.OutOfStock
    it "oos5" $ I._updateInventory sol (M.fromList [(1, 4), (2, 5)]) (M.fromList [(1, 5), (2, 3)]) `shouldBe` Left I.OutOfStock
    it "checkout ok" $ I._updateInventory sol (M.fromList [(1, 4), (2, 5)]) (M.fromList [(1, 2), (2, 3)]) `shouldBe` Right (M.fromList [(1, 2), (2, 2)])
    it "take all of an item" $ I._updateInventory sol (M.fromList [(1, 4), (2, 5)]) (M.fromList [(1, 4), (2, 3)]) `shouldBe` Right (M.fromList [(2, 2)])
  describe "annotatePrice" $ do
    it "empty basket" $ I._annotatePrice sol I.sampleCatalog mempty `shouldBe` Right mempty
    it "known prices" $ I._annotatePrice sol I.sampleCatalog (M.fromList [(1, 5), (2, 8)]) `shouldBe` Right (M.fromList [(1, (5, 785)), (2, (8, 200))])
    it "unknown price" $ I._annotatePrice sol I.sampleCatalog (M.fromList [(1, 5), (8, 8)]) `shouldBe` Left I.DescriptionNotFound
  describe "computePrice" $ do
    it "no rebates" $ I._computePrice sol [] (M.fromList [(1, (5, 8)), (56, (12, 32))]) `shouldBe` (5 * 8 + 12 * 32)
  describe "checkout" $ do
    describe "no rebates" $ do
      it "empty basket" $
        I._checkout sol I.sampleCatalog [] I.sampleInventory mempty `shouldBe` Right (I.sampleInventory, 0)
      it "out of stock" $
        I._checkout sol I.sampleCatalog [] I.sampleInventory (M.singleton 2 5)
          `shouldBe` Left I.OutOfStock
      it "buy all items" $
        I._checkout sol I.sampleCatalog [] I.sampleInventory (M.singleton 2 4)
          `shouldBe` Right (M.delete 2 I.sampleInventory, 4 * 200)
      it "something that has no description, but is in stock" $
        I._checkout sol I.sampleCatalog [] (M.insert 12 1 I.sampleInventory) (M.singleton 12 1)
          `shouldBe` Left I.DescriptionNotFound
      it "buy it all" $
        I._checkout sol I.sampleCatalog [] I.sampleInventory I.sampleInventory
          `shouldBe` Right (mempty, 12 * 785 + 4 * 200 + 8 * 24700 + 1 * 19700 + 43234 * 1500)
    describe "basic rebate" $ do
      let basket = M.fromList [(1, 3), (4, 1)]
          remaining =
            M.fromList
              [ (1, 9),
                (2, 4),
                (3, 8),
                (5, 43234)
              ]
      it "one item on sale" $
        I._checkout sol I.sampleCatalog [I.Rebate 1 700] I.sampleInventory basket
          `shouldBe` Right (remaining, 3 * 700 + 1 * 19700)
      it "two items on sale" $
        I._checkout sol I.sampleCatalog [I.Rebate 1 700, I.Rebate 4 5000] I.sampleInventory basket
          `shouldBe` Right (remaining, 3 * 700 + 1 * 5000)
      it "bad sale" $
        I._checkout sol I.sampleCatalog [I.Rebate 1 800] I.sampleInventory basket
          `shouldBe` Right (remaining, 3 * 785 + 1 * 19700)
      it "one good, and one bad sale" $
        I._checkout sol I.sampleCatalog [I.Rebate 1 700, I.Rebate 4 50000] I.sampleInventory basket
          `shouldBe` Right (remaining, 3 * 700 + 1 * 19700)
    describe "grouped rebate" $ do
      let basket = M.fromList [(1, 8)]
          remaining =
            M.fromList
              [ (1, 4),
                (2, 4),
                (3, 8),
                (4, 1),
                (5, 43234)
              ]
      it "simple grouped rebate" $
        I._checkout sol I.sampleCatalog [I.Grouped 1 8 1000] I.sampleInventory basket
          `shouldBe` Right (remaining, 1000)
      it "bad grouped rebate" $
        I._checkout sol I.sampleCatalog [I.Grouped 1 8 100000] I.sampleInventory basket
          `shouldBe` Right (remaining, 8 * 785)
      it "multi rebate 1" $
        I._checkout sol I.sampleCatalog [I.Grouped 1 4 1000] I.sampleInventory basket
          `shouldBe` Right (remaining, 2000)
      it "multi rebate 2" $
        I._checkout sol I.sampleCatalog [I.Grouped 1 5 3500, I.Grouped 1 3 2000] I.sampleInventory basket
          `shouldBe` Right (remaining, 3500 + 2000)
      it "mixed rebates" $
        I._checkout sol I.sampleCatalog [I.Grouped 1 5 6500, I.Grouped 1 3 2000] I.sampleInventory basket
          `shouldBe` Right (remaining, 2 * 785 + 4000)
    describe "nth free" $ do
      let basket = M.fromList [(1, 8)]
          remaining =
            M.fromList
              [ (1, 4),
                (2, 4),
                (3, 8),
                (4, 1),
                (5, 43234)
              ]
      it "simple nth" $
        I._checkout sol I.sampleCatalog [I.NthFree 1 7] I.sampleInventory basket
          `shouldBe` Right (remaining, 7 * 785)
    describe "LEVEL UP" $ do
      let basket = M.fromList [(1, 8)]
          remaining =
            M.fromList
              [ (1, 4),
                (2, 4),
                (3, 8),
                (4, 1),
                (5, 43234)
              ]
      it "bad" $
        I._checkout sol I.sampleCatalog [I.Grouped (-1) 6 1000, I.Grouped 133 2 500] I.sampleInventory basket
          `shouldBe` Right (remaining, 8 * 785)
      it "same product id, grouped" $
        I._checkout sol I.sampleCatalog [I.Grouped 1 6 1000, I.Grouped 1 2 500] I.sampleInventory basket
          `shouldBe` Right (remaining, 1500)
      it "same product id, mixed" $
        I._checkout sol I.sampleCatalog [I.Grouped 1 5 1000, I.NthFree 1 2] I.sampleInventory basket
          `shouldBe` Right (remaining, 1000 + 2 * 785)
      it "same product id, mixed, useless" $
        I._checkout sol I.sampleCatalog [I.Grouped 1 6 1000, I.NthFree 1 2] I.sampleInventory basket
          `shouldBe` Right (remaining, 1000 + 2 * 785)
      it "same product, size X" $
        I._checkout sol I.sampleCatalog (map (I.Rebate 1 . I.Price) [500 .. 520]) I.sampleInventory basket
          `shouldBe` Right (remaining, 500 * 8)
      it "same product, size XL" $ do
        -- pendingWith "uncomment this if you feel confident"
        I._checkout sol I.sampleCatalog (map (I.Rebate 1 . I.Price) (reverse [500 .. 600])) I.sampleInventory basket
          `shouldBe` Right (remaining, 500 * 8)
      it "same product, size XXL" $ do
        -- pendingWith "uncomment this if you feel confident"
        I._checkout sol I.sampleCatalog (map (I.Rebate 1 . I.Price) [500 .. 1000]) I.sampleInventory basket
          `shouldBe` Right (remaining, 500 * 8)
