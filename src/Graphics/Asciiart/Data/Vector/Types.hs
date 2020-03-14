{-|
Module      : Graphics.Asciiart.Data.Vector.Type
Description : Provide common types for Vector AAs
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This module Provides Common types for Vector AAs
Currently, it include: Vector, MathVector
-}
module Graphics.Asciiart.Data.Vector.Type where

-- | Style of graphics
--
data LetterType = OneChar Char -- ^ Use one character for every point
                | TinLine      -- ^ Use tin line like '\//\_-='
                | Bold         -- ^ Use 'qpdbz=#'



