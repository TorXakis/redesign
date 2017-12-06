{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sort
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the sorts.
-----------------------------------------------------------------------------
module Sort
(
  Sort(..)
)
where

-- | Sorts
data  Sort  = Bool
            | Int
            | Char
            | Regex
            | String
  deriving (Eq, Ord, Show)

-- | Is sort an AlgebraicDataType?
isAdt :: SortRef -> Bool
isAdt SortAdt{} = True
isAdt _         = False