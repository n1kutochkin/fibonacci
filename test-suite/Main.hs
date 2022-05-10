import Test.Tasty
import Test.Tasty.QuickCheck

import Lib

newtype Tiny = Tiny Integer deriving (Eq, Ord, Show)

instance Arbitrary Tiny where
  arbitrary = Tiny . fromIntegral <$> chooseInt (1, 30)
  shrink (Tiny x) = Tiny <$> shrink x

main :: IO ()
main = do
  defaultMain qctests

qctests :: TestTree
qctests = testGroup "QuickCheck" [
    testProperty "fibN"              $ prop_fibNTiny fibN
  , testProperty "fibNTailRecursive" $ prop_fibN fibNTailRecursive
  , testProperty "fibNaive"          $ prop_fib fibNaive
  , testProperty "fib"               $ prop_fib fib
  ]
  where
  prop_fibNTiny f (Tiny i) = f (i+2) == f i + f (i+1)
  prop_fibN f (Positive (Small i)) = f (i+2) == f i + f (i+1)
  prop_fib f (Positive i) =
    let [a,b,c] = take 3 $ drop i f
    in c == a + b
