module Monoids where

import           Data.Monoid    (Sum (..), getSum)
import           Data.Semigroup (Max (..), getMax)

-- | Custom data structure
data RequestStats = RequestStats
  { totalRequests :: Sum Int     -- Uses addition to combine
  , totalLatency  :: Sum Int     -- Uses addition to combine
  , maxLatency    :: Max Int     -- Uses "greater than" to combine
  } deriving (Show, Eq)

-- | First, it must be a Semigroup (the requirement for having <>)
instance Semigroup RequestStats where
  (RequestStats c1 t1 m1) <> (RequestStats c2 t2 m2) =
    RequestStats (c1 <> c2) (t1 <> t2) (m1 <> m2)

-- | Now it can be a Monoid (defining the empty state)
instance Monoid RequestStats where
  mempty = RequestStats mempty mempty mempty
  -- This effectively creates: RequestStats (Sum 0) (Sum 0) (Max minBound)

-- | Helper to convert a single raw latency into our Monoid
toStats :: Int -> RequestStats
toStats latency = RequestStats
  { totalRequests = Sum 1           -- Each hit is 1 request
  , totalLatency  = Sum latency     -- Add this latency to total
  , maxLatency    = Max latency     -- Check if this is the new max
  }

-- | The raw data (e.g., from a log file)
rawLogData :: [Int]
rawLogData = [20, 45, 12, 500, 30]

-- | THE MAGIC: Process everything in one go
-- >>> finalStats
-- RequestStats {totalRequests = Sum {getSum = 5}, totalLatency = Sum {getSum = 607}, maxLatency = Max {getMax = 500}}
finalStats :: RequestStats
finalStats = foldMap toStats rawLogData

main :: IO ()
main = do
  print finalStats
