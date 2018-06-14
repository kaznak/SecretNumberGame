module SecretNumberGame
    (
      numSecrets
    , SecretNum(..)
    , Hint(..)

    , mkSecretNum
    , choiceSecretNum
    , checkSecret
    ) where

import Data.Set (fromList, elemAt, deleteAt)
import Data.List
import Control.Monad(sequence)

import System.Random

------------------------------------------------------------------------
-- | Number of Secret digits.
numSecrets :: Int
numSecrets = 4

-- | Secret digits
--
-- prop> length $ secrets $ sn::SecretNum = numSecrets
--
data SecretNum = SN { secrets :: [Int] -- ^ the list of secret numbers
                    } deriving(Show)

data Hint = Hint { strike :: Int
                 , ball   :: Int
                 } deriving(Show)

------------------------------------------------------------------------
-- | Number Generator
mkSecretNum
  :: Int       -- ^ seed number
  -> SecretNum
mkSecretNum seed = choiceSecretNum pn
  where (pn, _) = randomR (0,(10 * 9 * 8 * 7 - 1)) (mkStdGen seed)

choiceSecretNum
  :: Int       -- ^ problem number
  -> SecretNum
choiceSecretNum pn | pn < 0                 = undefined
choiceSecretNum pn | pn >= (10 * 9 * 8 * 7) = undefined
choiceSecretNum pn =
  let n1 = div pn (9 * 8 * 7)
      m1 = mod pn (9 * 8 * 7)
      n2 = div m1 (8 * 7)
      m2 = mod m1 (8 * 7)
      n3 = div m2 7
      m3 = mod m2 7
      n4 = m3
      toks = fromList [0..9]
  in SN $ takeInds [] toks [n1,n2,n3,n4]
  where
    takeInds ret _ [] = ret
    takeInds r t (i:is) = takeInds ((elemAt i t):r) (deleteAt i t) is

------------------------------------------------------------------------
checkSecret
  :: SecretNum
  -> [Int]
  -> Hint
checkSecret sn ns = Hint (calc isHit) (calc isBall)
  where calc f = sum $ map fromEnum $ map (f sn ns) [0..3]

isHit
  :: SecretNum
  -> [Int]
  -> Int
  -> Bool
isHit sn ns index =
  ((secrets sn) !! index) == (ns !! index)

isBall
  :: SecretNum
  -> [Int]
  -> Int
  -> Bool
isBall sn ns index =
  (not $ isHit sn ns index) &&
  (elem (ns !! index) (secrets sn))
