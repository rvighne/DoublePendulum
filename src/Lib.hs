{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Prelude hiding (length)
import Data.List (intercalate)
import Debug.Trace
import Graphics.Gloss

data Pendulum v = Pendulum
  { mass :: v
  , length :: v
  , cm :: v
  , moment :: v
  , theta :: v
  , omega :: v }
  deriving Show

data DoublePendulum v = DoublePendulum (Pendulum v) (Pendulum v)
  deriving Show

toCSV :: Show v => DoublePendulum v -> String
toCSV (DoublePendulum p1 p2) = intercalate "," $ (show . theta) <$> [p1, p2]

accel :: Floating f => f -> DoublePendulum f -> (f, f)
accel g (DoublePendulum p1 p2) = ((d * b1 - b * b2) / det, (-c * b1 + a * b2) / det)
  where
    a = moment p1 + mass p2 * length p1 ^ 2
    b = mass p2 * length p1 * cm p2 * cos (theta p1 - theta p2)
    c = b
    d = moment p2 + mass p2 * cm p2 ^ 2
    det = a * d - b * c
    b1 = mass p2 * length p1 * cm p2 * omega p2 * (omega p1 - omega p2) * sin (theta p1 - theta p2) - mass p2 * length p1 * cm p2 * omega p1 * omega p2 * sin (theta p1 - theta p2) - mass p1 * g * cm p1 * sin (theta p1) - mass p2 * g * length p1 * sin (theta p1)
    b2 = mass p2 * length p1 * cm p2 * omega p1 * (omega p1 - omega p2) * sin (theta p1 - theta p2) + mass p2 * length p1 * cm p2 * omega p1 * omega p2 * sin (theta p1 - theta p2) - mass p2 * g * cm p2 * sin (theta p2)

step :: Floating f => f -> f -> DoublePendulum f -> DoublePendulum f
step g dt dp@(DoublePendulum p1 p2) = DoublePendulum (integrate a1 p1) (integrate a2 p2)
  where
    (a1, a2) = accel g dp
    integrate alpha p@Pendulum{theta, omega} = p
      { theta = theta + omega * dt + alpha * dt ^ 2 / 2
      , omega = omega + alpha * dt }

initial :: Floating f => DoublePendulum f
initial = DoublePendulum p1 p2
  where
    p1 = Pendulum
      { mass = 0.330
      , length = 0.298
      , cm = 0.0935
      , moment = 0.00554
      , theta = 1.0
      , omega = 5 }
    p2 = Pendulum
      { mass = 0.315
      , length = 0.298
      , cm = 0.0935
      , moment = 0.00534
      , theta = -0.2
      , omega = 1 }

outputData :: IO ()
outputData = mapM_ (putStrLn . toCSV) $ take n $ iterate (step g dt) initial
  where
    n = truncate $ 10.0 / dt
    g = 9.81
    dt = 0.01

runSimulation :: IO ()
runSimulation = simulate (InWindow "Double Pendulum" (400, 400) (10, 10)) white freq initial render (\_ -> step 9.81)
  where
    freq = 100000
    render (DoublePendulum p1 p2) = scale pxMeter pxMeter $ pictures
      [ rotate (screenAngle $ theta p1) r1
      , translate (-length p1 * sin (theta p1)) (-length p1 * cos (theta p1)) $ rotate (screenAngle $ theta p2) r2 ]
      where
        r1 = color blue $ rectangleUpperSolid thickness $ length p1
        r2 = color red $ rectangleUpperSolid thickness $ length p2
        screenAngle theta = 180 + theta * 180 / pi
        thickness = 0.05
        pxMeter = 250

demo :: IO ()
demo = runSimulation
