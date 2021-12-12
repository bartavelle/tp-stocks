module S2021.SampleStudent where

import Basics as B
import qualified Data.Map.Strict as M
import Inventory as I

-- PART 1 - basics

-- | Write a functions that adds two Int.
add :: Int -> Int -> Int
add = undefined

-- | Write a functions that divides two Int.
divide :: Int -> Int -> Int
divide = undefined

-- | Rewrite a function that divides two Ints, but that explicitely
-- fails when it divides by 0, or explicitely succeeds.
divide' :: Int -> Int -> IntResult
divide' = undefined

-- | Write this function again, but this time using the generic type.
divide'' :: Int -> Int -> Result Int
divide'' = undefined

-- | This is (5 * 3.4) + 8
-- run it in GHCi
example1 :: Int
example1 = eval (Add (Mul (Value 5) (Value 3)) (Value 8))

-- | What is this ?
example2 :: Int
example2 = eval (Div (Mul (Value 5) (Value 3)) (Value 0))

-- | From an operation, compute its value.
eval :: Operation -> Int
eval = undefined

-- | Try it in the REPL with example2!
-- Now write the correct function:
eval' :: Operation -> Result Int
eval' = undefined

-- | Examples to run in GHCi
example1' :: Result Int
example1' = eval' (Add (Mul (Value 5) (Value 3)) (Value 8))

example2' :: Result Int
example2' = eval' (Div (Mul (Value 5) (Value 3)) (Value 0))

-- PART 2 - lists

-- | Returns the first element of a list.
listHead :: List a -> Result a
listHead = undefined

head1 :: Result Int
head1 = listHead Empty -- Failure

head2 :: Result Int
head2 = listHead (Cons 5 (Cons 4 Empty)) -- success 5

-- | Returns the tail of a list.
listTail :: List a -> Result (List a)
listTail = undefined

tail1 :: Result (List Int)
tail1 = listTail Empty -- Failure

tail2 :: Result (List Int)
tail2 = listTail (Cons 5 (Cons 4 Empty)) -- success (Cons 4 Empty)

-- | Sum of all integers in a list.
listSum :: List Int -> Int
listSum = undefined

-- | Compare two lists for equality.
listEq :: Eq a => List a -> List a -> Bool
listEq = undefined

-- | Converts our list type into Haskell's built-in list type.
toList :: List a -> [a]
toList = undefined

-- Given a function, converts all elements of a list.
lmap ::
  (a -> b) ->
  List a ->
  List b
lmap = undefined

-- optional assignment
-- Uncomment the relevant test if you wrote it!
ltraverse ::
  Applicative f =>
  (a -> f b) ->
  List a ->
  f (List b)
ltraverse = undefined

-- PART 3 - homework

-- | Returns the name of an item description.
--
-- You need to use pattern matching.
getName :: ItemDesc -> String
getName = undefined

-- | Returns the price of an item description.
--
-- You need to use pattern matching.
getPrice :: ItemDesc -> Price
getPrice = undefined

-- | Returns the type of an item description.
--
-- You need to use pattern matching.
getType :: ItemDesc -> ItemType
getType = undefined

-- | Retrieves an item price from the catalog. Can fail if the item is
-- unknown.
getItemPrice ::
  Catalog ->
  ItemId ->
  Either Problem Price
getItemPrice = undefined

-- | Update the inventory by removing all items from the basket. This could
-- fail with `Left OutOfStock` if the inventory is not sufficiently
-- stocked.
updateInventory ::
  Inventory ->
  Basket ->
  Either Problem Inventory
updateInventory = undefined

-- | Given a catalog, and a basket, annotate prices of the basket
-- can fail if the description is not found!
annotatePrice :: Catalog -> Basket -> Either Problem (M.Map ItemId (Quantity, Price))
annotatePrice = undefined

-- | Given a list of rebates, and a shopping basket with prices attached,
-- return the total cost of the basket.
-- 
-- extra: take into account the rebates to find the best price possible
computePrice ::
  [Rebates] ->
  M.Map ItemId (Quantity, Price) ->
  Price
computePrice = undefined

-- | The checkout functions computes how much a customer should pay for
-- a given basket, and updates the inventory. It can also fail!
--
-- It is recommended to first fill the next functions, as they will be
-- quite useful for writing this one!
checkout ::
  -- | products catalog
  Catalog ->
  -- | currently available rebates
  [Rebates] ->
  -- | current inventory
  Inventory ->
  -- | customer basket
  Basket ->
  -- | if this succeeds, return the basket total cost and the updated inventory.
  Either Problem (Inventory, Price)
checkout = undefined

-- DO NOT EDIT THESE DEFINITIONS
isolution :: I.Solution
isolution = I.Solution getItemPrice updateInventory annotatePrice computePrice checkout getName getPrice getType

bsolution :: B.Solution
bsolution = B.Solution add divide divide' divide'' eval eval' listHead listTail listSum listEq toList lmap ltraverse