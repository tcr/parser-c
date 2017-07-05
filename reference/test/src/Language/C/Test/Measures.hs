{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Measures
-- Copyright   :  (c) Benedikt Huber 2008
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  non-portable (GeneralizedNewtypeDeriving)
--
-- Simple unit names and scaling for rendering performance measurements.
-- The main objective is to have easily serializable and renderable quantities,
-- with some type safety (e.g. 'Time').
-----------------------------------------------------------------------------
module Language.C.Test.Measures (
-- * simple performance measurements
PerfMeasure(..), elapsedTime, processedEntities,
-- * metric scaling
Scale(..),scaleFromUnit,scaleToUnit,
kilo,
MetricScale(..),metricUnitAbbr,
-- * formatting quantities 
formatQuantity, 
UnitDescr(..), unitDescription, formatUnits, formatUnitsSafe, formatUnitsAuto,
linesOfCode,topLevelDeclarations,
-- * formatting time
Time,picoSeconds,milliSeconds,seconds,
TimeScale,formatTime, formatTimeSafe, formatTimeAuto, formatSeconds, scaleSecs,
per,formatUnitsPerTime, formatUnitsPerSecond,
) where
import Numeric (showFFloat)

-- ============================
-- = Performance Measurements =
-- ============================
  
-- | Simple newtype for recording performance measurements @(entityCount,elapsedTime)@
newtype PerfMeasure = PerfMeasure (Integer , Time) deriving (Show, Read)

elapsedTime :: PerfMeasure -> Time
elapsedTime (PerfMeasure (_,t)) = t

processedEntities :: PerfMeasure -> Integer
processedEntities (PerfMeasure (sz,_)) = sz

-- ============
-- = Scaling  =
-- ============

class Scale s where
  scaleFactor :: s -> Double

scaleFromUnit :: (Scale s) => s -> Double -> Double
scaleFromUnit s n = n / (scaleFactor s)

scaleToUnit   :: (Scale s) => s -> Double -> Double
scaleToUnit s n = n * (scaleFactor s)

data MetricScale = Pico | Nano | Micro | Unit | Milli | Kilo | Mega | Giga deriving (Eq,Ord,Show,Read)

instance Scale MetricScale where
  scaleFactor Pico  = 1E-12
  scaleFactor Nano  = 1E-9
  scaleFactor Micro = 1E-6
  scaleFactor Milli = 1E-3
  scaleFactor Unit = 1
  scaleFactor Kilo  = 1E3
  scaleFactor Mega  = 1E6
  scaleFactor Giga  = 1E9
  
metricUnitAbbr :: MetricScale -> String -> String
metricUnitAbbr scale =
  case scale of
    Pico    -> ("p"++) 
    Nano    -> ("n"++)
    Micro   -> ("micro"++)
    Milli   -> ("m"++)
    Unit    -> id 
    Kilo    -> ("K"++)
    Mega    -> ("M"++)
    Giga    -> ("G"++)

autoScale :: (Floating a, Ord a) => a -> MetricScale
autoScale n = 
  case logBase 10 n of
    k | k < (-10.3) -> Pico
      | k < (-7.3)  -> Nano
      | k < (-4.3)  -> Micro
      | k < (-1.3)  -> Milli
      | k < 3       -> Unit
      | k < 5.7     -> Kilo
      | k < 8.7     -> Mega
      | otherwise   -> Giga

isDisplayable :: (Scale s, Real a) => s -> Int -> a -> Bool
isDisplayable scale significantDigits n =
  let nd = realToFrac n :: Double in
  scaleFromUnit scale nd > (10 ** (- (realToFrac $ significantDigits)))
  
formatQuantity :: (Real a) => Double -> a -> String -> String
formatQuantity scalefactor q measureAbbr 
  = (formatNum $ scalefactor * realToFrac q) ++ " " ++ measureAbbr
  where
    formatNum num = showFFloat (Just 2) num ""

-- ====================================
-- = Description of measurement units =
-- ====================================

-- | Unit Descriptions
data UnitDescr = UnitDescr { uAbbr :: String, uDescr :: String } deriving (Show,Read)

unitDescription :: String -> String -> UnitDescr
unitDescription abbr descr = UnitDescr abbr descr

-- | @formatUnits 3728 Kilo linesOfCode = "3.73 KLoC"@
formatUnits :: (Real a) => a -> MetricScale -> UnitDescr -> String
formatUnits n scale unitDescr = formatQuantity (1 / scaleFactor scale) n (metricUnitAbbr scale (uAbbr unitDescr))

-- |  Like formatUnits, but make sure to rescale if the value is 0 in the specified scale
--
--  @formatUnitsStd 3278 Kilo linesOfCode = "3.73 KLoC"@
--  @formatUnitsStd 5 Kilo linesOfCode = "5 LoC"@ 
formatUnitsSafe :: (Real a) => a -> MetricScale -> UnitDescr -> String
formatUnitsSafe n scale unitDescr | isDisplayable scale 2 n = formatUnitsAuto nd unitDescr
                                  | otherwise = formatUnits nd scale unitDescr
  where nd = (realToFrac n :: Double)

-- | Format some unit, choose best scale
formatUnitsAuto :: (Real a) => a -> UnitDescr -> String
formatUnitsAuto n unitDescr = formatUnits nd (autoScale nd) unitDescr
  where nd = (realToFrac n :: Double)

-- =====================
-- = Time measurements =
-- =====================                                  

-- | Time in seconds  
newtype Time = Time { tSecs :: Double } deriving (Real,Fractional,Num,Ord,Eq,Show,Read,Floating) 

picoSeconds :: (Real a) => a -> Time
picoSeconds ms = Time (scaleToUnit Pico (realToFrac ms))

milliSeconds :: (Real a) => a -> Time
milliSeconds ms = Time (scaleToUnit Milli (realToFrac ms))

seconds :: Double -> Time
seconds s = Time s

-- | Metric scales of time, i.e. milli, micro, nano seconds
--
-- TODO: add time-units minute, hour, day et.c
newtype TimeScale = MetricTime MetricScale deriving (Eq,Ord,Scale,Show,Read)

timeScaleAbbr :: TimeScale -> String
timeScaleAbbr (MetricTime ms) = metricUnitAbbr ms "s"

scaleSecs :: MetricScale -> TimeScale
scaleSecs = MetricTime

-- | @formatTime (picoseconds 10^12) (scaleSecs Milli) = "1000.00 ms"@
formatTime :: Time -> TimeScale -> String
formatTime t timescale = formatQuantity (1 / scaleFactor timescale) t (timeScaleAbbr timescale)

formatTimeSafe :: Time -> TimeScale -> String
formatTimeSafe t timescale | isDisplayable timescale 2 t  = formatTime t timescale
                           | otherwise                   = formatTimeAuto t

formatTimeAuto :: Time -> String
formatTimeAuto t = formatTime t  (scaleSecs $ autoScale t)

-- | @formatSeconds (0.1) = "0.1 s"@
-- | @formatSeconds (0.001) = "10 ms"@ (To avoid "0.00 s") (TODO: generalize)
formatSeconds :: Time -> String
formatSeconds t | t <= 0.01 = formatTime t (MetricTime Milli)
                | otherwise = formatTime t (MetricTime Unit)

-- ==============
-- = Throughput =
-- ==============

-- Example: (kilo 5) `per` (picoSeconds 3)
per :: (Real a) => a -> Time -> Double
per n t = (realToFrac n / tSecs t)

-- | @formatUnitsPerTime (kilo 5 `per` picoSeconds 1^10) Kilo linesOfCode (secs Milli) = "50000.00  KLoC / ms@
formatUnitsPerTime :: Double -> MetricScale -> UnitDescr -> TimeScale -> String
formatUnitsPerTime upt mScale unitDescr tScale = 
       formatQuantity scalefactor upt (metricUnitAbbr mScale (uAbbr unitDescr))
    ++ " / "
    ++ timeScaleAbbr tScale
  where
    scalefactor = (scaleFactor tScale) / (scaleFactor mScale)

formatUnitsPerSecond :: Double -> MetricScale -> UnitDescr -> String
formatUnitsPerSecond upt mScale unitDescr = formatUnitsPerTime upt mScale unitDescr (scaleSecs Unit)

-- ========================
-- = Few common units     =
-- ========================
linesOfCode :: UnitDescr
linesOfCode = unitDescription "LoC" "Lines of Code"
topLevelDeclarations :: UnitDescr
topLevelDeclarations = unitDescription "Decls" "top level declarations"
kilo        :: Integer -> Double
kilo n = scaleToUnit Kilo (realToFrac n)

-- in-source examples
{-
-- 3.27 KLoC
_ex1 :: String
_ex1 = formatUnits (3728::Int) Kilo linesOfCode
-- 1000.00 ms
_ex2 :: String
_ex2 = formatTime (picoSeconds ((10::Integer) ^ 12 ::Integer)) (scaleSecs Milli)
-- ("2.50 LoC / ms", "2.50 KLoC / s")
_ex3 :: (String,String)
_ex3 = 
  let throughput = (50 `per` milliSeconds 20) in
  ( formatUnitsPerTime throughput Unit linesOfCode (scaleSecs Milli)
  , formatUnitsPerTime throughput Kilo linesOfCode (scaleSecs Unit))
-}