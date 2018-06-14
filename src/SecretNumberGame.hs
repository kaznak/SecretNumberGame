module SecretNumberGame
    (
      numSecrets
    , SecretNum(..)

    , mkSecretNum
    , checkSecret
    ) where

import Data.Set (Set(..), fromList, size, elemAt, deleteAt)
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
mkSecretNum seed = SN secrets
  where (secrets, _, _) =
          pickRandomNTok' 4 [] (fromList [0..9]) (mkStdGen seed)

pickRandomNTok'
  :: (RandomGen g) =>
     Int       -- ^ Number of Tokens
  -> [Int]     -- ^ Token picked
  -> Set Int   -- ^ Tokens
  -> g         -- ^ Random Generator
  -> ( [Int]   -- ^ Token picked
     , Set Int -- ^ Token left
     , g       -- ^ Random Generator
     )
pickRandomNTok' 0 tp toks gen     = (tp, toks, gen)
pickRandomNTok' ntoks tp toks gen =
  let (tokpicked, nexttoks, nextgen) = pickRandomTok' toks gen
  in  pickRandomNTok' (ntoks - 1) (tokpicked : tp) nexttoks nextgen

pickRandomTok'
  :: (RandomGen g) =>
     Set Int   -- ^ Tokens
  -> g         -- ^ Random Generator
  -> ( Int     -- ^ Token picked
     , Set Int -- ^ Token left
     , g       -- ^ Random Generator
     )
pickRandomTok' toks gen =
  let (index, newgen) = randomR (0,(size toks)-1) gen
  in  ( elemAt index toks
      , deleteAt index toks
      , newgen
      )

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
  ((secrets sn) !! index /= ns !! index) &&
  (elem (ns !! index) (secrets sn))
