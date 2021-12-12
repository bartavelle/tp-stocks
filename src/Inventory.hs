{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inventory where

--
--   DO NOT EDIT THIS FILE
--

import qualified Data.Map.Strict as M

data ItemType
  = Clothing
  | HighTech
  | Book
  deriving (Show, Eq, Ord, Enum, Bounded)

newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Num)

newtype Quantity = Quantity Int
  deriving (Show, Eq, Ord, Num)

newtype Price = Price Integer
  deriving (Show, Eq, Ord, Num)

data ItemDesc
  = ItemDesc
      String -- name
      Price -- cost
      ItemType -- item type
  deriving (Show, Eq)

-- | A catalog maps item identifiers (key) to item descriptions (values).
type Catalog = M.Map ItemId ItemDesc

-- | An inventory maps item identifiers to quantities.
-- Absent entries (when the map does not hold the item identifier) mean
-- there is no inventory.
type Inventory = M.Map ItemId Quantity

-- | The shopping basket has the same type as the inventory.
type Basket = M.Map ItemId Quantity

sampleCatalog :: Catalog
sampleCatalog =
  M.fromList
    [ (1, ItemDesc "Neuromancer" 785 Book),
      (2, ItemDesc "Rashômon" 200 Book),
      (3, ItemDesc "Huawei P20 Lite" 24700 HighTech),
      (4, ItemDesc "Xiaomi Redmi Note 5" 19700 HighTech),
      (5, ItemDesc "WearAll - Femmes Tricoté Longue Manche Renne Noël 3D Chandail Dames Flocon De Neige Cavalier" 1500 Clothing)
    ]

sampleInventory :: Inventory
sampleInventory =
  M.fromList
    [ (1, 12),
      (2, 4),
      (3, 8),
      (4, 1),
      (5, 43234)
    ]

-- | Rebates! There are three types of rebates:
data Rebates
  = -- | The item price is modified.
    --   For example, if item 3 costs 85€, the rebate will be:
    --   > Rebate 3 85
    Rebate ItemId Price
  | -- | Item bundles with a discount price.
    -- For example, with the item #5, if buying a bundle of 3 costs 150€,
    -- the rebate will be:
    -- > Grouped 5 3 150
    Grouped ItemId Quantity Price
  | -- | If you buy N items, the next is free.
    -- For example, for item #5, if the 4th is free (meaning you get
    -- 4 items for the price of 3), the rebase will be:
    -- > NthFree 5 3
    NthFree ItemId Quantity
  deriving (Show)

-- Rebates are not automatically applied. The combination that is the
-- most advantageous to the customer should be selected among all
-- possibilities!

-- | Problems that could occur when running the checkout procedure
data Problem
  = OutOfStock
  | DescriptionNotFound
  deriving (Show, Eq)

-- | Use this function to compute the price. You should not do it yourself
-- as, if time allows, we will refactor the whole thing later with proper
-- types.
cost :: Quantity -> Price -> Price
cost (Quantity q) (Price p) = Price (fromIntegral q * p)

data Solution = Solution
  { _getItemPrice :: Catalog -> ItemId -> Either Problem Price,
    _updateInventory :: Inventory -> Basket -> Either Problem Inventory,
    _annotatePrice :: Catalog -> Basket -> Either Problem (M.Map ItemId (Quantity, Price)),
    _computePrice :: [Rebates] -> M.Map ItemId (Quantity, Price) -> Price,
    _checkout :: Catalog -> [Rebates] -> Inventory -> Basket -> Either Problem (Inventory, Price),
    _getName :: ItemDesc -> String,
    _getPrice :: ItemDesc -> Price,
    _getType :: ItemDesc -> ItemType
  }
