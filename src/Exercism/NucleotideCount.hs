{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Exercism.NucleotideCount (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import Data.Map qualified as Map (empty, insertWith)
import Protolude (readEither)
import Data.Either.Combinators (mapLeft)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Bounded, Enum,Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = count <$> traverse charToRnaNucleotide xs
  where
    count = foldr (\x -> Map.insertWith (+) x 1) Map.empty
    charToRnaNucleotide = \case
      'A' -> Right A
      'C' -> Right C
      'G' -> Right G
      'T' -> Right T
      c -> Left $ "Invalid nucleotide " <> show c
            <> "( valid nucleotides: " <> show ([minBound..maxBound]::[Nucleotide]) <> ")"

nucleotideCounts2 :: String -> Either String (Map Nucleotide Int)
nucleotideCounts2 xs = foldr (flip (Map.insertWith (+)) 1 ) Map.empty 
                          <$> traverse (\a -> mapLeft (const ( "bad nucleotide:" <>  [a])) $ readEither [a]) xs

-- >>> nucleotideCounts2 "GATTACA"
-- >>> nucleotideCounts2 "INVALID"
-- Right (fromList [(A,3),(C,1),(G,1),(T,2)])
-- Left "bad nucleotide:I"
