-- | ListLike instances for several @Data.Vector@ types.
-- The `Data.ListLike.Vector.Generic` instances are not exported from this
-- module in order to prevent collisions.
--
module Data.ListLike.Vector (
  module Data.ListLike.Vector.Storable
 ,module Data.ListLike.Vector.Unboxed
 ,module Data.ListLike.Vector.Vector
)

where

import Data.ListLike.Vector.Storable
import Data.ListLike.Vector.Unboxed
import Data.ListLike.Vector.Vector
