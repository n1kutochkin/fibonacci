import System.Clock
import Control.DeepSeq
import Control.Exception

import Lib

main :: IO ()
main = do
  bench "fibN 35"              $ fibN 35
  bench "fibNTailRecursive 90" $ fibNTailRecursive 90
  bench "fibNaive 90"          $ take 90 fibNaive
  bench "fib 90"               $ take 90 fib
  where
  bench n x = do
    putStrLn n
    m1 <- getTime MonotonicRaw
    c1 <- getTime ProcessCPUTime
    () <- pure $ rnf x
    m2 <- getTime MonotonicRaw
    c2 <- getTime ProcessCPUTime
    putStrLn $ "\tMonotonic: " <> show (toNanoSecs $ m2 - m1) <> "ns"
    putStrLn $ "\tCPU:       " <> show (toNanoSecs $ c2 - c1) <> "ns"
    putStrLn ""
    `catch` (\e -> print (e :: SomeException))
