{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wdeferred-out-of-scope-variables #-}
module Exercism.RNATranscription (toRNA) where

import Data.Map
import Data.Ord
import Data.Maybe ()
import Data.Either(Either(..))

data DNA = DnaG | DnaC | DnaT | DnaA deriving Eq
data RNA = RnaC | RnaG | RnaA | RnaU deriving Eq

toRNA :: String -> Either Char String
toRNA = traverse (fmap rnaNucleotideToChar . charToRnaNucleotide)
        where
            charToRnaNucleotide 'A' = Right $ dnaNucleotideToRna DnaA
            charToRnaNucleotide 'C' = Right $ dnaNucleotideToRna DnaC
            charToRnaNucleotide 'G' = Right $ dnaNucleotideToRna DnaG
            charToRnaNucleotide 'T' = Right $ dnaNucleotideToRna DnaT
            charToRnaNucleotide v = Left v

            dnaNucleotideToRna nucleotide = case nucleotide of
                                             DnaA -> RnaU
                                             DnaC -> RnaG
                                             DnaG -> RnaC
                                             DnaT -> RnaA

            rnaNucleotideToChar nucleotide = case nucleotide of
                                             RnaU -> 'U'
                                             RnaG -> 'G'
                                             RnaC -> 'C'
                                             RnaA -> 'A'           