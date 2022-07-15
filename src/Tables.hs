module Tables where

import Prelude hiding (lookup)

-- START HERE AFTER reaching the pointer in Datatypes.hs

newtype Table k v = Table [(k, v)]
  deriving (Show)

-- In the following, we first reimplement the functions
-- from the slides, but with the @newtype@-based version
-- of the 'Table' type.

-- Task Tables-1.
--
-- Re-implement 'empty'.

empty :: Table k v
empty = Table []

-- Task Tables-2.
--
-- Re-implement 'insert'.

insert :: Eq k => k -> v -> Table k v -> Table k v
insert k v (Table t) = Table ((k, v) : filter (\(k', _) -> k /= k') t)

-- Task Tables-3.
--
-- Re-implement 'delete'.

delete :: Eq k => k -> Table k v -> Table k v
delete _ (Table []) = Table []
delete k (Table ((k', v) : t))
  | k == k' = delete k (Table t)
  | otherwise = insert k' v (delete k (Table t))

-- Task Tables-4.
--
-- Re-implement 'lookup'.

lookup :: Eq k => k -> Table k v -> Maybe v
lookup _ (Table []) = Nothing
lookup k (Table ((k', v) : t))
  | k == k' = Just v
  | otherwise = lookup k (Table t)

-- Task Tables-5.
--
-- Implement a map function on the table values.

mapValues :: Eq k => (v1 -> v2) -> Table k v1 -> Table k v2
mapValues _ (Table []) = Table []
mapValues f (Table ((k, v) : t)) = insert k (f v) (mapValues f (Table t))

-- Task Tables-6.
--
-- Implement a map function on the table keys.
--
-- Tricky additional question:
-- Can you identify a potential problem with
-- this function?

mapKeys :: Eq k1 => Eq k2 => (k1 -> k2) -> Table k1 v -> Table k2 v
mapKeys _ (Table []) = Table []
mapKeys f (Table ((k, v) : t)) = insert (f k) v (mapKeys f (Table t))

-- Task Tables-7.
--
-- Implement a more general table update function.
-- The function 'alter' takes a function and a key.

alter :: Eq k => (v -> v) -> k -> Table k v -> Table k v
alter _ _ (Table []) = Table []
alter f k (Table ((k', v) : t)) =
  if k == k'
    then alter f k (insert k fv (Table t))
    else alter f k (insert k v (Table t))
  where
    fv = f v

-- Task Tables-8.
--
-- Add an export list to the module, exporting
-- all the functions, and the 'Table' type, but
-- no constructors. The syntax
--
--   Table()
--
-- can be used in the export list to export a
-- datatype or newtype without any of its
-- constructors.

-- GO TO Transactions.hs
