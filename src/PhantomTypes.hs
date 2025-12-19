{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PhantomTypes (demo) where

-- Phantom tags (no values of these types are ever constructed)
data Meter
data Foot

newtype Quantity u = Quantity Double
  deriving (Eq, Ord, Num, Fractional, Show)

meters :: Double -> Quantity Meter
meters = Quantity

feet :: Double -> Quantity Foot
feet = Quantity

-- Addition only works for the same unit
add :: Quantity u -> Quantity u -> Quantity u
add = (+)

-- Unit-safe conversion functions (explicit)
feetToMeters :: Quantity Foot -> Quantity Meter
feetToMeters (Quantity x) = Quantity (x * 0.3048)

metersToFeet :: Quantity Meter -> Quantity Foot
metersToFeet (Quantity x) = Quantity (x / 0.3048)

demo :: IO ()
demo = do
  let h1 = meters 2.0
      h2 = meters 1.5
      totalM = add h1 h2           -- OK
      totalF = metersToFeet totalM -- OK
  print totalM
  print totalF
  -- add h1 (feet 3)               -- Compile error: units don't match
