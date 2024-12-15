{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module FraJer.Par
  ( happyError
  , myLexer
  , pInstr
  , pExpr
  , pStmt
  , pDef
  , pParams
  , pArgs
  , pLambda
  , pSType
  , pFType
  ) where

import Prelude

import qualified FraJer.Abs
import FraJer.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap12 = HappyWrap12 (FraJer.Abs.Ident)
happyIn12 :: (FraJer.Abs.Ident) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (Integer)
happyIn13 :: (Integer) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 (FraJer.Abs.SType)
happyIn14 :: (FraJer.Abs.SType) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (FraJer.Abs.FType)
happyIn15 :: (FraJer.Abs.FType) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (FraJer.Abs.Expr)
happyIn16 :: (FraJer.Abs.Expr) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (FraJer.Abs.Expr)
happyIn17 :: (FraJer.Abs.Expr) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (FraJer.Abs.Expr)
happyIn18 :: (FraJer.Abs.Expr) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (FraJer.Abs.Args)
happyIn19 :: (FraJer.Abs.Args) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (FraJer.Abs.Params)
happyIn20 :: (FraJer.Abs.Params) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (FraJer.Abs.Lambda)
happyIn21 :: (FraJer.Abs.Lambda) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (FraJer.Abs.Instr)
happyIn22 :: (FraJer.Abs.Instr) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (FraJer.Abs.Instr)
happyIn23 :: (FraJer.Abs.Instr) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (FraJer.Abs.Def)
happyIn24 :: (FraJer.Abs.Def) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (FraJer.Abs.SpecStmt)
happyIn25 :: (FraJer.Abs.SpecStmt) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (FraJer.Abs.Def)
happyIn26 :: (FraJer.Abs.Def) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (FraJer.Abs.Stmt)
happyIn27 :: (FraJer.Abs.Stmt) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (FraJer.Abs.Stmt)
happyIn28 :: (FraJer.Abs.Stmt) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\xe0\x87\x43\x82\x1a\x11\x00\x00\x00\x88\x10\x01\x00\x00\x10\x00\x20\x18\x00\x00\x00\x00\x00\x00\x00\x60\x90\xa0\x42\x04\x00\x00\x00\x00\x00\x00\xfc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x00\x01\x00\x00\x00\x00\x80\x08\x11\x00\x24\x00\x01\x00\x86\x01\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x44\x00\x00\x01\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x06\x00\x80\x00\x80\x00\x08\x00\x00\x00\xc0\x08\x20\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x88\x00\x00\x00\x08\x00\x10\x0c\x00\x00\x00\x22\x44\x00\x00\x00\x04\x00\x08\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x88\x44\x22\x80\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x00\x40\x00\x40\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\xe1\x90\xa0\x46\x04\x00\x00\x00\x00\x00\x00\xfc\x70\x48\x50\x23\x02\x00\x00\x00\x00\x00\x00\x7e\x38\x24\xa8\x11\x01\x00\x00\x80\x08\x11\x00\x00\x00\x01\x00\x82\x01\x00\x00\x40\x84\x08\x00\x00\x80\x00\x00\xc1\x00\x00\x00\x20\x42\x04\x00\x00\x40\x00\x80\x60\x00\x00\x00\x10\x21\x02\x00\x00\x20\x00\x40\x30\x00\x00\x00\x88\x10\x01\x00\x00\x10\x00\x20\x18\x00\x00\x00\x44\x88\x00\x00\x00\x08\x00\x10\x0c\x00\x00\x00\x22\x44\x00\x00\x00\x04\x00\x08\x06\x00\x00\x00\x11\x22\x00\x00\x00\x02\x00\x04\x03\x00\x00\x80\x08\x11\x00\x00\x00\x01\x00\x82\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x21\x02\x00\x00\x20\x00\x40\x30\x00\x00\x00\x88\x10\x01\x00\x00\x10\x00\x20\x18\x00\x00\x00\x44\x88\x00\x00\x00\x08\x00\x10\x0c\x00\x00\x00\x22\x44\x00\x00\x00\x04\x00\x08\x06\x00\x00\x00\x11\x22\x00\x00\x00\x02\x00\x04\x03\x00\x00\x80\x08\x11\x00\x00\x00\x01\x00\x82\x01\x00\x00\x40\x84\x08\x00\x00\x80\x00\x00\xc1\x00\x00\x00\x20\x42\x04\x00\x00\x40\x00\x80\x60\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x44\x00\x00\x08\x00\x08\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x22\x00\x48\x00\x02\x00\x0c\x03\x00\x00\x80\x08\x11\x00\x00\x00\x01\x00\x82\x01\x00\x00\x40\x84\x08\x00\x00\x80\x00\x00\xc1\x00\x00\x00\x20\x42\x04\x00\x00\x40\x00\x80\x60\x00\x00\x00\x10\x21\x02\x00\x00\x20\x00\x40\x30\x00\x00\x00\x88\x10\x01\x00\x00\x10\x00\x20\x18\x00\x00\x00\x44\x88\x00\x00\x00\x08\x00\x10\x0c\x00\x00\x00\x22\x44\x00\x00\x00\x04\x00\x08\x06\x00\x00\x00\x11\x22\x00\x00\x00\x02\x00\x04\x03\x00\x00\x80\x08\x11\x00\x00\x00\x01\x00\x82\x01\x00\x00\x40\x84\x08\x00\x12\x80\x00\x00\xc3\x00\x00\x00\x20\x42\x04\x00\x09\x40\x00\x80\x61\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x88\x00\x00\x00\x08\x00\x10\x0c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x10\x21\x02\x00\x00\x20\x00\x40\x30\x00\x00\x00\x00\x88\x00\x00\x18\x00\x10\x00\x01\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x0d\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x06\x00\x10\x00\x00\x00\x00\x00\x88\x10\x01\x00\x00\x10\x00\x20\x18\x00\x00\x00\x00\x00\x00\xb0\x01\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x11\x00\x00\x00\x01\x00\x82\x01\x00\x00\x00\x40\x04\x00\xc0\x00\x80\x00\x08\x00\x00\x00\x00\x20\x02\x00\x40\x00\x40\x00\x04\x00\x00\x00\x00\x10\x01\x00\x20\x00\x20\x00\x02\x00\x00\x00\x00\x88\x00\x00\x10\x00\x10\x00\x01\x00\x00\x00\x00\x44\x00\x00\x08\x00\x08\x80\x00\x00\x00\x00\x00\x22\x00\x00\x04\x00\x04\x40\x00\x00\x00\x00\x00\x11\x00\x00\x02\x00\x02\x20\x00\x00\x00\x00\x90\x08\x00\x00\x01\x00\x01\x10\x00\x00\x00\x40\x84\x08\x00\x00\x80\x00\x00\xc1\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x01\x00\x20\x00\x20\x00\x02\x00\x00\x00\x00\x89\x00\x00\x10\x00\x10\x00\x01\x00\x00\x00\x80\x44\x00\x00\x08\x00\x08\x80\x00\x00\x00\x00\x40\x22\x00\x00\x04\x00\x04\x40\x00\x00\x00\x00\x46\x00\xd9\x01\x00\x00\x00\x00\x00\x00\x00\x00\x23\x80\xec\x00\x00\x00\x00\x00\x00\x00\x00\x80\x11\x40\x76\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x08\x20\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x60\x04\x90\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x44\x88\x00\x00\x00\x08\x00\x10\x0c\x00\x00\x00\x40\x22\x00\x00\x04\x00\x04\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x04\x00\xc0\x00\x80\x00\x08\x00\x00\x00\x20\x42\x04\x00\x00\x40\x00\x80\x60\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x00\x00\x10\x00\x10\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x04\x00\xc0\x00\x80\x00\x08\x00\x00\x00\x20\x42\x04\x00\x00\x40\x00\x80\x60\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x0c\x00\x08\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x80\x08\x00\x80\x01\x00\x01\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x00\x00\x10\x00\x10\x10\x01\x00\x00\x00\x00\x00\x00\xf8\xe1\x90\xa0\x46\x04\x00\x00\x00\x00\x00\x00\xfc\x70\x48\x50\x23\x02\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x10\x21\x02\x00\x00\x20\x00\x40\x30\x00\x00\x00\x88\x10\x01\x00\x00\x10\x00\x20\x18\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x38\x24\xa8\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\xc0\x0f\x87\x04\x35\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x88\x10\x01\x00\x00\x10\x00\x20\x18\x00\x00\x00\x44\x88\x00\x00\x00\x08\x00\x10\x0c\x00\x00\x00\x40\x22\x00\x00\x04\x00\x04\x40\x00\x00\x00\x00\x20\x11\x00\x00\x02\x00\x02\x20\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x44\x00\x00\x08\x00\x08\x80\x00\x00\x00\x00\x40\x22\x00\x00\x04\x00\x04\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xc3\x21\x41\x8d\x08\x00\x00\x00\x00\x00\x00\xf8\xe1\x90\xa0\x46\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pInstr","%start_pExpr","%start_pStmt","%start_pDef","%start_pParams","%start_pArgs","%start_pLambda","%start_pSType","%start_pFType","Ident","Integer","SType","FType","Expr2","Expr","Expr1","Args","Params","Lambda","Instr","Instr1","Def","SpecStmt","Def1","Stmt1","Stmt","'!'","'!='","'%'","'%='","'('","')'","'*'","'*='","'+'","'++'","'+='","','","'-'","'--'","'-='","'->'","'/'","'/='","';'","'<'","'<='","'='","'=='","'>'","'>='","'Array'","'Bool'","'BoolFunc'","'Dict'","'Int'","'IntFunc'","'['","']'","'and'","'assignment'","'break'","'continue'","'debug'","'disable'","'else'","'enable'","'false'","'for'","'get'","'has'","'if'","'key'","'lambda'","'none'","'or'","'outer'","'print'","'reading'","'return'","'set'","'skip'","'swap'","'to'","'true'","'void'","'while'","'xor'","'{'","'}'","L_Ident","L_integ","%eof"]
        bit_start = st Prelude.* 95
        bit_end = (st Prelude.+ 1) Prelude.* 95
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..94]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xef\x00\x03\x00\xfb\x00\x3d\x01\x71\x00\x01\x00\xf7\xff\x2b\x00\x35\x00\xc2\xff\x00\x00\xc6\xff\x00\x00\x00\x00\xc6\xff\x00\x00\x00\x00\xda\xff\xcb\xff\xda\x00\x00\x00\x00\x00\x6c\x00\x32\x01\xcb\xff\x13\x00\x03\x00\x03\x00\xe5\xff\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\xe5\xff\xeb\xff\x00\x00\xf3\xff\xf3\xff\xf4\xff\x00\x00\x45\x00\x45\x00\xe5\x00\x00\x00\xf5\xff\x3a\x00\x14\x00\x46\x00\x56\x00\x57\x00\x64\x00\x00\x00\x6e\x00\xf8\xff\x0c\x00\x85\x00\x87\x00\x8c\x00\x00\x00\xed\xff\x75\x00\x41\x00\x3e\x00\x6a\x00\xef\x00\xef\x00\xef\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x41\x00\x9f\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x88\x00\x68\x00\x68\x00\x00\x00\xac\x00\x9d\x00\xb6\x00\xbb\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x01\x00\x01\x00\x00\x00\x00\x00\x03\x00\xad\x00\xa6\x00\xcc\x00\x71\x00\xb7\x00\x03\x00\x74\x00\xd4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x71\x00\x71\x00\x03\x00\x71\x00\xbe\x00\x00\x00\x03\x00\x7a\x00\xb4\x00\xb4\x00\xb4\x00\xb4\x00\xb4\x00\xb4\x00\x0f\x00\x03\x00\xc7\x00\x18\x00\x26\x00\x2c\x00\x4a\x00\x32\x01\x32\x01\x32\x01\x32\x01\x32\x01\x00\x00\x00\x00\x00\x00\xa1\x00\xa1\x00\xa1\x00\xa1\x00\xdb\x00\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\x00\x00\xb2\x00\x03\x00\x4c\x00\x00\x00\xd6\x00\x94\x00\x03\x00\xfe\x00\xb4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x00\x03\x00\x02\x01\xff\x00\xa2\x00\x00\x00\xd3\x00\xa7\x00\xdf\x00\x19\x01\x00\x00\xb1\x00\xef\x00\xef\x00\x1b\x01\x00\x00\xe2\x00\xe4\x00\x03\x00\x03\x00\x28\x01\x2d\x01\xef\x00\x00\x00\xf7\x00\xef\x00\xfd\x00\x03\x00\x03\x00\x54\x00\x59\x00\x0f\x01\x00\x00\x09\x01\x15\x01\x00\x00\x62\x00\x67\x00\x00\x00\x01\x01\x00\x00\x00\x00\x00\x00\xef\x00\xef\x00\x25\x01\x2b\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xf3\x00\xca\x00\x19\x00\xab\x00\x6f\x00\xdb\x01\x09\x00\x6b\x01\x67\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x3a\x01\x76\x01\x7b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x01\x7e\x01\x00\x00\x00\x00\x87\x01\x8c\x01\x00\x00\x00\x00\x8b\x01\x8d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x01\x00\x00\x00\x00\x42\x01\x53\x01\x64\x01\xab\x02\xb2\x02\xb9\x02\xc0\x02\xc7\x02\x03\x02\x0a\x02\x11\x02\x18\x02\x9d\x01\x00\x00\x1f\x02\x26\x02\x2d\x02\x34\x02\x3b\x02\x42\x02\x49\x02\x50\x02\x00\x00\x9e\x01\xa0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\x01\xf8\x00\x17\x01\x2a\x01\x68\x01\x79\x01\x8a\x01\x9b\x01\xac\x01\xbd\x01\xef\x01\xf9\x01\x00\x00\x00\x00\x57\x02\x00\x00\x00\x00\x00\x00\x7d\x00\x00\x00\x5e\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbd\x00\xd9\x00\x65\x02\x0e\x01\x00\x00\x00\x00\x6c\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa9\x01\xae\x01\xaf\x01\xb1\x01\x00\x00\xba\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x81\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x01\x86\x01\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x02\x96\x02\x00\x00\x00\x00\x97\x01\x00\x00\x00\x00\xa8\x01\x00\x00\x9d\x02\xa4\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\x01\xca\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\x00\x00\xf1\xff\xf2\xff\x00\x00\xf3\xff\xf4\xff\x00\x00\x00\x00\xef\xff\xee\xff\xd9\xff\xd1\xff\xe0\xff\x00\x00\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\xe6\xff\xd2\xff\xf5\xff\x00\x00\x00\x00\x00\x00\xcd\xff\x00\x00\x00\x00\x00\x00\xc0\xff\x00\x00\x00\x00\x00\x00\xa1\xff\x00\x00\xac\xff\xaa\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb0\xff\x00\x00\x00\x00\x00\x00\xc4\xff\x00\x00\x00\x00\xc3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\xc1\xff\xc2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\x00\x00\x00\x00\xcb\xff\xcc\xff\xea\xff\xec\xff\x00\x00\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xed\xff\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\xd6\xff\xd4\xff\xd8\xff\xd7\xff\xd5\xff\xdc\xff\xdb\xff\xda\xff\xd3\xff\xce\xff\xe7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb5\xff\x00\x00\x00\x00\xa9\xff\xa5\xff\xa7\xff\xa8\xff\xa6\xff\xa4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xdf\xff\xde\xff\xe1\xff\xe2\xff\xc7\xff\xc6\xff\xc5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbd\xff\xbc\xff\xbb\xff\xba\xff\x00\x00\xaf\xff\xae\xff\x00\x00\x00\x00\x00\x00\xad\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb9\xff\xc9\xff\xca\xff\xf0\xff\xe9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xab\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\xff\xb2\xff\x00\x00\x00\x00\xa3\xff\x00\x00\x00\x00\xb8\xff\x00\x00\xc8\xff\xb7\xff\xa2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb1\xff\xb4\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x09\x00\x01\x00\x41\x00\x01\x00\x0d\x00\x05\x00\x13\x00\x05\x00\x43\x00\x30\x00\x0a\x00\x03\x00\x0a\x00\x43\x00\x0e\x00\x23\x00\x0e\x00\x09\x00\x1c\x00\x06\x00\x06\x00\x1f\x00\x09\x00\x09\x00\x00\x00\x22\x00\x0d\x00\x0d\x00\x1c\x00\x06\x00\x0c\x00\x1f\x00\x09\x00\x35\x00\x00\x00\x01\x00\x0d\x00\x41\x00\x04\x00\x0f\x00\x10\x00\x32\x00\x2a\x00\x06\x00\x2a\x00\x43\x00\x09\x00\x22\x00\x22\x00\x06\x00\x0d\x00\x41\x00\x09\x00\x3e\x00\x43\x00\x43\x00\x0d\x00\x22\x00\x43\x00\x3b\x00\x3c\x00\x3b\x00\x05\x00\x32\x00\x32\x00\x41\x00\x42\x00\x41\x00\x42\x00\x1b\x00\x33\x00\x22\x00\x1e\x00\x32\x00\x05\x00\x3e\x00\x3e\x00\x22\x00\x43\x00\x06\x00\x1c\x00\x06\x00\x09\x00\x1f\x00\x09\x00\x3e\x00\x0d\x00\x32\x00\x0d\x00\x06\x00\x05\x00\x05\x00\x09\x00\x32\x00\x06\x00\x1b\x00\x0d\x00\x09\x00\x1e\x00\x3e\x00\x27\x00\x0d\x00\x29\x00\x06\x00\x05\x00\x3e\x00\x09\x00\x22\x00\x06\x00\x22\x00\x0d\x00\x09\x00\x02\x00\x03\x00\x05\x00\x0d\x00\x09\x00\x22\x00\x08\x00\x0c\x00\x0d\x00\x05\x00\x22\x00\x32\x00\x09\x00\x32\x00\x02\x00\x03\x00\x0d\x00\x41\x00\x09\x00\x22\x00\x08\x00\x32\x00\x0d\x00\x3e\x00\x22\x00\x3e\x00\x32\x00\x1b\x00\x1c\x00\x22\x00\x1e\x00\x1f\x00\x27\x00\x3e\x00\x29\x00\x32\x00\x21\x00\x22\x00\x3e\x00\x13\x00\x32\x00\x13\x00\x21\x00\x22\x00\x09\x00\x32\x00\x13\x00\x3e\x00\x0d\x00\x31\x00\x09\x00\x05\x00\x3e\x00\x32\x00\x0d\x00\x20\x00\x41\x00\x3e\x00\x09\x00\x32\x00\x02\x00\x03\x00\x0d\x00\x09\x00\x05\x00\x3e\x00\x16\x00\x0d\x00\x21\x00\x22\x00\x0c\x00\x3e\x00\x0e\x00\x09\x00\x21\x00\x22\x00\x09\x00\x0d\x00\x02\x00\x03\x00\x0d\x00\x0c\x00\x21\x00\x22\x00\x08\x00\x32\x00\x0c\x00\x21\x00\x22\x00\x00\x00\x01\x00\x32\x00\x20\x00\x04\x00\x05\x00\x06\x00\x05\x00\x3e\x00\x22\x00\x32\x00\x2f\x00\x22\x00\x20\x00\x3e\x00\x32\x00\x06\x00\x02\x00\x03\x00\x16\x00\x20\x00\x05\x00\x3e\x00\x08\x00\x41\x00\x32\x00\x0a\x00\x3e\x00\x32\x00\x0c\x00\x0e\x00\x04\x00\x41\x00\x3a\x00\x16\x00\x08\x00\x3f\x00\x3e\x00\x0b\x00\x3f\x00\x3e\x00\x00\x00\x0f\x00\x02\x00\x03\x00\x12\x00\x00\x00\x01\x00\x20\x00\x16\x00\x04\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x06\x00\x20\x00\x2c\x00\x2d\x00\x06\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x10\x00\x02\x00\x03\x00\x3f\x00\x24\x00\x25\x00\x26\x00\x08\x00\x00\x00\x01\x00\x3a\x00\x2b\x00\x04\x00\x37\x00\x2e\x00\x05\x00\x24\x00\x25\x00\x06\x00\x40\x00\x34\x00\x40\x00\x36\x00\x2b\x00\x38\x00\x39\x00\x2e\x00\x00\x00\x01\x00\x3d\x00\x05\x00\x04\x00\x34\x00\x41\x00\x36\x00\x05\x00\x38\x00\x02\x00\x03\x00\x3f\x00\x28\x00\x3d\x00\x07\x00\x00\x00\x01\x00\x41\x00\x40\x00\x04\x00\x05\x00\x06\x00\x40\x00\x00\x00\x11\x00\x02\x00\x03\x00\x14\x00\x15\x00\x3f\x00\x17\x00\x18\x00\x19\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x3f\x00\x02\x00\x03\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x40\x00\x02\x00\x03\x00\x00\x00\x01\x00\x03\x00\x40\x00\x04\x00\x02\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x01\x00\x00\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x01\x00\x00\x00\x02\x00\x04\x00\x02\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x01\x00\x00\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x01\x00\x00\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\xff\xff\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\x09\x00\x00\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\x09\x00\x00\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\x09\x00\x00\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\x09\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\x06\x00\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x46\x00\x1b\x00\x0b\x00\x1b\x00\x47\x00\x1c\x00\x5c\x00\x1c\x00\xff\xff\x76\x00\x1d\x00\x11\x00\x1d\x00\xff\xff\x1e\x00\x41\x00\x1e\x00\x12\x00\x0d\x00\x87\x00\xb5\x00\x0e\x00\x46\x00\x46\x00\x2c\x00\x48\x00\x47\x00\x47\x00\x0d\x00\xb2\x00\x65\x00\x0e\x00\x46\x00\x42\x00\x13\x00\x14\x00\x47\x00\x0b\x00\x63\x00\x2d\x00\x2e\x00\x49\x00\x1f\x00\xb1\x00\x1f\x00\xff\xff\x46\x00\x48\x00\x48\x00\xb0\x00\x47\x00\x0b\x00\x46\x00\x4a\x00\xff\xff\xff\xff\x47\x00\x48\x00\xff\xff\x20\x00\x21\x00\x20\x00\x51\x00\x49\x00\x49\x00\x0b\x00\x22\x00\x0b\x00\x22\x00\x10\x00\x50\x00\x48\x00\x11\x00\x49\x00\x4f\x00\x4a\x00\x4a\x00\x48\x00\xff\xff\xaf\x00\x0d\x00\xc9\x00\x46\x00\x0e\x00\x46\x00\x4a\x00\x47\x00\x49\x00\x47\x00\xe2\x00\x4e\x00\x4d\x00\x46\x00\x49\x00\xe1\x00\x10\x00\x47\x00\x46\x00\x11\x00\x4a\x00\xa7\x00\x47\x00\xa8\x00\xe9\x00\x4c\x00\x4a\x00\x46\x00\x48\x00\xe8\x00\x48\x00\x47\x00\x46\x00\x22\x00\x23\x00\x4b\x00\x47\x00\x46\x00\x48\x00\x24\x00\x6f\x00\x47\x00\x40\x00\x48\x00\x49\x00\x46\x00\x49\x00\x22\x00\x23\x00\x47\x00\x0b\x00\x46\x00\x48\x00\xc0\x00\x49\x00\x47\x00\x4a\x00\x48\x00\x4a\x00\x49\x00\x10\x00\x0d\x00\x48\x00\x11\x00\x0e\x00\xa5\x00\x4a\x00\xa6\x00\x49\x00\xbe\x00\x48\x00\x4a\x00\x45\x00\x49\x00\x44\x00\xb6\x00\x48\x00\x46\x00\x49\x00\x43\x00\x4a\x00\x47\x00\x26\x00\x46\x00\x97\x00\x4a\x00\x49\x00\x47\x00\x8e\x00\x0b\x00\x4a\x00\x46\x00\x49\x00\x26\x00\x27\x00\x47\x00\x46\x00\x8b\x00\x4a\x00\x8a\x00\x47\x00\xc7\x00\x48\x00\x28\x00\x4a\x00\x29\x00\x46\x00\xc4\x00\x48\x00\x46\x00\x47\x00\x22\x00\x23\x00\x47\x00\x89\x00\xd6\x00\x48\x00\xbb\x00\x49\x00\x88\x00\xd4\x00\x48\x00\x13\x00\x14\x00\x49\x00\x79\x00\x15\x00\x37\x00\x17\x00\x77\x00\x4a\x00\x48\x00\x49\x00\x78\x00\x48\x00\xc0\x00\x4a\x00\x49\x00\xbd\x00\x22\x00\x23\x00\xb3\x00\xb8\x00\x70\x00\x4a\x00\xba\x00\x0b\x00\x49\x00\x71\x00\x4a\x00\x49\x00\xaa\x00\x72\x00\x52\x00\x0b\x00\xd1\x00\xc8\x00\x53\x00\xcc\x00\x4a\x00\x54\x00\xcb\x00\x4a\x00\x2c\x00\x55\x00\x26\x00\x27\x00\x56\x00\x13\x00\x14\x00\x73\x00\x57\x00\x84\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\xc5\x00\x58\x00\x74\x00\x75\x00\xc2\x00\x2b\x00\x10\x00\x0d\x00\x2c\x00\x11\x00\x0e\x00\xd7\x00\x22\x00\x23\x00\xd5\x00\x30\x00\x31\x00\x3e\x00\xb8\x00\x13\x00\x14\x00\xd3\x00\x32\x00\x83\x00\x59\x00\x33\x00\xd2\x00\x30\x00\x31\x00\xce\x00\xdf\x00\x34\x00\xde\x00\x35\x00\x32\x00\x36\x00\x3f\x00\x33\x00\x13\x00\x14\x00\x37\x00\xdb\x00\x82\x00\x34\x00\x0b\x00\x35\x00\xda\x00\x36\x00\x66\x00\x67\x00\xd8\x00\xe0\x00\x37\x00\x68\x00\x13\x00\x14\x00\x0b\x00\xe5\x00\x15\x00\x62\x00\x17\x00\xe7\x00\x2c\x00\x69\x00\x26\x00\x27\x00\x6a\x00\x6b\x00\xeb\x00\x6c\x00\x6d\x00\x6e\x00\xa3\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x2c\x00\xea\x00\x26\x00\x27\x00\x2b\x00\x10\x00\x0d\x00\x2c\x00\x11\x00\x0e\x00\xa2\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x2c\x00\xef\x00\x26\x00\x27\x00\x13\x00\x14\x00\x0b\x00\xee\x00\x81\x00\x0e\x00\xa1\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x2c\x00\x61\x00\x26\x00\x27\x00\x13\x00\x14\x00\x60\x00\x5f\x00\x80\x00\x5e\x00\xcf\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x2c\x00\x5d\x00\x26\x00\x27\x00\x13\x00\x14\x00\x5c\x00\x5a\x00\x7f\x00\x59\x00\xce\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x2c\x00\xa8\x00\x26\x00\x27\x00\x13\x00\x14\x00\x97\x00\x8c\x00\x7e\x00\x8b\x00\xd8\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x2c\x00\xad\x00\x26\x00\x27\x00\x13\x00\x14\x00\xac\x00\xab\x00\x7d\x00\xaa\x00\xe5\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x2c\x00\xcc\x00\x26\x00\x27\x00\x13\x00\x14\x00\x00\x00\x00\x00\x7c\x00\x00\x00\xec\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x2c\x00\x00\x00\x26\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\x00\x39\x00\x3a\x00\x3b\x00\x29\x00\x2d\x00\x3c\x00\x13\x00\x14\x00\x00\x00\x11\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x19\x00\x13\x00\x14\x00\x00\x00\x11\x00\x15\x00\x16\x00\x17\x00\x85\x00\x00\x00\x19\x00\x13\x00\x14\x00\x00\x00\x11\x00\x15\x00\x16\x00\x17\x00\x7b\x00\x00\x00\x19\x00\x13\x00\x14\x00\x00\x00\x11\x00\x15\x00\x16\x00\x17\x00\x7a\x00\x00\x00\x19\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x9b\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x9a\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x99\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x98\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x95\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x94\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x93\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x92\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x91\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x90\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x8f\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x8e\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x79\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xbe\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xb9\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xb6\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xb3\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xc9\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xc5\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xc2\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xdc\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xdb\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xe3\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\xe2\x00\x17\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x00\x00\xa0\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x00\x00\x9f\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x00\x00\x9e\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x00\x00\x9d\x00\x13\x00\x14\x00\x00\x00\x00\x00\x15\x00\x00\x00\x9c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (9, 94) [
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94)
	]

happy_n_terms = 68 :: Prelude.Int
happy_n_nonterms = 17 :: Prelude.Int

happyReduce_9 = happySpecReduce_1  0# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn12
		 (FraJer.Abs.Ident happy_var_1
	)}

happyReduce_10 = happySpecReduce_1  1# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn13
		 ((read happy_var_1) :: Integer
	)}

happyReduce_11 = happySpecReduce_1  2# happyReduction_11
happyReduction_11 happy_x_1
	 =  happyIn14
		 (FraJer.Abs.STInt
	)

happyReduce_12 = happySpecReduce_1  2# happyReduction_12
happyReduction_12 happy_x_1
	 =  happyIn14
		 (FraJer.Abs.STBool
	)

happyReduce_13 = happySpecReduce_1  3# happyReduction_13
happyReduction_13 happy_x_1
	 =  happyIn15
		 (FraJer.Abs.FTInt
	)

happyReduce_14 = happySpecReduce_1  3# happyReduction_14
happyReduction_14 happy_x_1
	 =  happyIn15
		 (FraJer.Abs.FTBool
	)

happyReduce_15 = happyReduce 4# 4# happyReduction_15
happyReduction_15 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn16
		 (FraJer.Abs.FuncVal happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_16 = happySpecReduce_1  4# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn16
		 (FraJer.Abs.VarVal happy_var_1
	)}

happyReduce_17 = happySpecReduce_1  4# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn16
		 (FraJer.Abs.ENum happy_var_1
	)}

happyReduce_18 = happySpecReduce_2  4# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn16
		 (FraJer.Abs.EPostInc happy_var_1
	)}

happyReduce_19 = happySpecReduce_2  4# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn16
		 (FraJer.Abs.EPreInc happy_var_2
	)}

happyReduce_20 = happySpecReduce_2  4# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn16
		 (FraJer.Abs.EPostDec happy_var_1
	)}

happyReduce_21 = happySpecReduce_2  4# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn16
		 (FraJer.Abs.EPreDec happy_var_2
	)}

happyReduce_22 = happyReduce 4# 4# happyReduction_22
happyReduction_22 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn16
		 (FraJer.Abs.EArray happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_23 = happyReduce 5# 4# happyReduction_23
happyReduction_23 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	happyIn16
		 (FraJer.Abs.EDict happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_24 = happySpecReduce_3  4# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	happyIn16
		 (happy_var_2
	)}

happyReduce_25 = happySpecReduce_1  4# happyReduction_25
happyReduction_25 happy_x_1
	 =  happyIn16
		 (FraJer.Abs.BTrue
	)

happyReduce_26 = happySpecReduce_1  4# happyReduction_26
happyReduction_26 happy_x_1
	 =  happyIn16
		 (FraJer.Abs.BFalse
	)

happyReduce_27 = happySpecReduce_2  4# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { (HappyWrap16 happy_var_2) -> 
	happyIn16
		 (FraJer.Abs.BNot happy_var_2
	)}

happyReduce_28 = happyReduce 6# 4# happyReduction_28
happyReduction_28 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_5 of { (HappyWrap17 happy_var_5) -> 
	happyIn16
		 (FraJer.Abs.BDictHasKey happy_var_1 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_29 = happySpecReduce_3  5# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	happyIn17
		 (FraJer.Abs.EPlus happy_var_1 happy_var_3
	)}}

happyReduce_30 = happySpecReduce_3  5# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	happyIn17
		 (FraJer.Abs.EMinus happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_1  5# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn17
		 (happy_var_1
	)}

happyReduce_32 = happySpecReduce_3  5# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	happyIn17
		 (FraJer.Abs.BOr happy_var_1 happy_var_3
	)}}

happyReduce_33 = happySpecReduce_3  5# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	happyIn17
		 (FraJer.Abs.BAnd happy_var_1 happy_var_3
	)}}

happyReduce_34 = happySpecReduce_3  5# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	happyIn17
		 (FraJer.Abs.BXor happy_var_1 happy_var_3
	)}}

happyReduce_35 = happySpecReduce_3  6# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.EDiv happy_var_1 happy_var_3
	)}}

happyReduce_36 = happySpecReduce_3  6# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.EMul happy_var_1 happy_var_3
	)}}

happyReduce_37 = happySpecReduce_3  6# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.EMod happy_var_1 happy_var_3
	)}}

happyReduce_38 = happySpecReduce_1  6# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn18
		 (happy_var_1
	)}

happyReduce_39 = happySpecReduce_3  6# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.EEq happy_var_1 happy_var_3
	)}}

happyReduce_40 = happySpecReduce_3  6# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.ELeq happy_var_1 happy_var_3
	)}}

happyReduce_41 = happySpecReduce_3  6# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.EGeq happy_var_1 happy_var_3
	)}}

happyReduce_42 = happySpecReduce_3  6# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.ELt happy_var_1 happy_var_3
	)}}

happyReduce_43 = happySpecReduce_3  6# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.EGt happy_var_1 happy_var_3
	)}}

happyReduce_44 = happySpecReduce_3  6# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn18
		 (FraJer.Abs.ENeq happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_1  7# happyReduction_45
happyReduction_45 happy_x_1
	 =  happyIn19
		 (FraJer.Abs.ArgsVoid
	)

happyReduce_46 = happySpecReduce_1  7# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	happyIn19
		 (FraJer.Abs.ArgsOne happy_var_1
	)}

happyReduce_47 = happySpecReduce_3  7# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn19
		 (FraJer.Abs.ArgsMany happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_1  7# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn19
		 (FraJer.Abs.ArgsLambda happy_var_1
	)}

happyReduce_49 = happySpecReduce_3  7# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn19
		 (FraJer.Abs.ArgsLambdaMany happy_var_1 happy_var_3
	)}}

happyReduce_50 = happySpecReduce_1  8# happyReduction_50
happyReduction_50 happy_x_1
	 =  happyIn20
		 (FraJer.Abs.ParamsNone
	)

happyReduce_51 = happySpecReduce_2  8# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn20
		 (FraJer.Abs.ParamVar happy_var_1 happy_var_2
	)}}

happyReduce_52 = happySpecReduce_2  8# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	happyIn20
		 (FraJer.Abs.ParamFunc happy_var_1 happy_var_2
	)}}

happyReduce_53 = happyReduce 4# 8# happyReduction_53
happyReduction_53 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut20 happy_x_4 of { (HappyWrap20 happy_var_4) -> 
	happyIn20
		 (FraJer.Abs.ParamVarMany happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_54 = happyReduce 4# 8# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut20 happy_x_4 of { (HappyWrap20 happy_var_4) -> 
	happyIn20
		 (FraJer.Abs.ParamFuncMany happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_55 = happyReduce 9# 9# happyReduction_55
happyReduction_55 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut20 happy_x_4 of { (HappyWrap20 happy_var_4) -> 
	case happyOut22 happy_x_8 of { (HappyWrap22 happy_var_8) -> 
	happyIn21
		 (FraJer.Abs.Lam happy_var_1 happy_var_4 happy_var_8
	) `HappyStk` happyRest}}}

happyReduce_56 = happySpecReduce_3  10# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	happyIn22
		 (FraJer.Abs.ISeq happy_var_1 happy_var_3
	)}}

happyReduce_57 = happySpecReduce_3  10# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	happyIn22
		 (FraJer.Abs.IDSeq happy_var_1 happy_var_3
	)}}

happyReduce_58 = happySpecReduce_3  10# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	happyIn22
		 (FraJer.Abs.ISSeq happy_var_1 happy_var_3
	)}}

happyReduce_59 = happySpecReduce_1  10# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_60 = happySpecReduce_1  11# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	happyIn23
		 (FraJer.Abs.IStmt happy_var_1
	)}

happyReduce_61 = happySpecReduce_2  11# happyReduction_61
happyReduction_61 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_62 = happySpecReduce_2  12# happyReduction_62
happyReduction_62 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_63 = happySpecReduce_1  12# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_64 = happySpecReduce_2  13# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_65 = happyReduce 6# 13# happyReduction_65
happyReduction_65 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut12 happy_x_5 of { (HappyWrap12 happy_var_5) -> 
	happyIn25
		 (FraJer.Abs.SSwap happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_66 = happyReduce 4# 13# happyReduction_66
happyReduction_66 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	happyIn25
		 (FraJer.Abs.DebugAssEnable happy_var_4
	) `HappyStk` happyRest}

happyReduce_67 = happyReduce 4# 13# happyReduction_67
happyReduction_67 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	happyIn25
		 (FraJer.Abs.DebugAssDisable happy_var_4
	) `HappyStk` happyRest}

happyReduce_68 = happyReduce 4# 13# happyReduction_68
happyReduction_68 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	happyIn25
		 (FraJer.Abs.DebugReadEnable happy_var_4
	) `HappyStk` happyRest}

happyReduce_69 = happyReduce 4# 13# happyReduction_69
happyReduction_69 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	happyIn25
		 (FraJer.Abs.DebugReadDisable happy_var_4
	) `HappyStk` happyRest}

happyReduce_70 = happyReduce 4# 14# happyReduction_70
happyReduction_70 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	happyIn26
		 (FraJer.Abs.VarDef happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_71 = happyReduce 8# 14# happyReduction_71
happyReduction_71 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut20 happy_x_4 of { (HappyWrap20 happy_var_4) -> 
	case happyOut22 happy_x_7 of { (HappyWrap22 happy_var_7) -> 
	happyIn26
		 (FraJer.Abs.FuncDef happy_var_1 happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_72 = happyReduce 9# 14# happyReduction_72
happyReduction_72 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut17 happy_x_5 of { (HappyWrap17 happy_var_5) -> 
	case happyOut17 happy_x_8 of { (HappyWrap17 happy_var_8) -> 
	happyIn26
		 (FraJer.Abs.ArrDefInit happy_var_2 happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_73 = happyReduce 6# 14# happyReduction_73
happyReduction_73 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut17 happy_x_5 of { (HappyWrap17 happy_var_5) -> 
	happyIn26
		 (FraJer.Abs.ArrDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_74 = happySpecReduce_3  14# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn26
		 (FraJer.Abs.DictDef happy_var_2 happy_var_3
	)}}

happyReduce_75 = happyReduce 11# 15# happyReduction_75
happyReduction_75 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	case happyOut22 happy_x_6 of { (HappyWrap22 happy_var_6) -> 
	case happyOut22 happy_x_10 of { (HappyWrap22 happy_var_10) -> 
	happyIn27
		 (FraJer.Abs.sif1 happy_var_3 happy_var_6 happy_var_10
	) `HappyStk` happyRest}}}

happyReduce_76 = happyReduce 7# 15# happyReduction_76
happyReduction_76 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	case happyOut22 happy_x_6 of { (HappyWrap22 happy_var_6) -> 
	happyIn27
		 (FraJer.Abs.sif2 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_77 = happyReduce 7# 15# happyReduction_77
happyReduction_77 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	case happyOut22 happy_x_6 of { (HappyWrap22 happy_var_6) -> 
	happyIn27
		 (FraJer.Abs.SWhile happy_var_3 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_78 = happyReduce 11# 15# happyReduction_78
happyReduction_78 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut17 happy_x_5 of { (HappyWrap17 happy_var_5) -> 
	case happyOut17 happy_x_7 of { (HappyWrap17 happy_var_7) -> 
	case happyOut22 happy_x_10 of { (HappyWrap22 happy_var_10) -> 
	happyIn27
		 (FraJer.Abs.SFor happy_var_3 happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest}}}}

happyReduce_79 = happySpecReduce_1  15# happyReduction_79
happyReduction_79 happy_x_1
	 =  happyIn27
		 (FraJer.Abs.SSkip
	)

happyReduce_80 = happyReduce 4# 15# happyReduction_80
happyReduction_80 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.SReturn happy_var_3
	) `HappyStk` happyRest}

happyReduce_81 = happyReduce 4# 15# happyReduction_81
happyReduction_81 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.SPrint happy_var_3
	) `HappyStk` happyRest}

happyReduce_82 = happyReduce 4# 15# happyReduction_82
happyReduction_82 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.SBreak happy_var_3
	) `HappyStk` happyRest}

happyReduce_83 = happySpecReduce_1  15# happyReduction_83
happyReduction_83 happy_x_1
	 =  happyIn27
		 (FraJer.Abs.SBreak1
	)

happyReduce_84 = happyReduce 5# 15# happyReduction_84
happyReduction_84 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	happyIn27
		 (FraJer.Abs.SContinue happy_var_4
	) `HappyStk` happyRest}

happyReduce_85 = happySpecReduce_1  15# happyReduction_85
happyReduction_85 happy_x_1
	 =  happyIn27
		 (FraJer.Abs.SContinue0
	)

happyReduce_86 = happySpecReduce_3  15# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.VarAssign happy_var_1 happy_var_3
	)}}

happyReduce_87 = happySpecReduce_3  15# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.VarAssignPlus happy_var_1 happy_var_3
	)}}

happyReduce_88 = happySpecReduce_3  15# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.VarAssignMinus happy_var_1 happy_var_3
	)}}

happyReduce_89 = happySpecReduce_3  15# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.VarAssignMul happy_var_1 happy_var_3
	)}}

happyReduce_90 = happySpecReduce_3  15# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.VarAssignDiv happy_var_1 happy_var_3
	)}}

happyReduce_91 = happySpecReduce_3  15# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn27
		 (FraJer.Abs.VarAssignMod happy_var_1 happy_var_3
	)}}

happyReduce_92 = happyReduce 8# 15# happyReduction_92
happyReduction_92 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	case happyOut17 happy_x_7 of { (HappyWrap17 happy_var_7) -> 
	happyIn27
		 (FraJer.Abs.ArrElSet happy_var_1 happy_var_3 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_93 = happyReduce 9# 15# happyReduction_93
happyReduction_93 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	case happyOut17 happy_x_8 of { (HappyWrap17 happy_var_8) -> 
	happyIn27
		 (FraJer.Abs.DictElSet happy_var_1 happy_var_4 happy_var_8
	) `HappyStk` happyRest}}}

happyReduce_94 = happySpecReduce_1  16# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn28
		 (happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 67# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (TS _ 53) -> cont 53#;
	PT _ (TS _ 54) -> cont 54#;
	PT _ (TS _ 55) -> cont 55#;
	PT _ (TS _ 56) -> cont 56#;
	PT _ (TS _ 57) -> cont 57#;
	PT _ (TS _ 58) -> cont 58#;
	PT _ (TS _ 59) -> cont 59#;
	PT _ (TS _ 60) -> cont 60#;
	PT _ (TS _ 61) -> cont 61#;
	PT _ (TS _ 62) -> cont 62#;
	PT _ (TS _ 63) -> cont 63#;
	PT _ (TS _ 64) -> cont 64#;
	PT _ (TV happy_dollar_dollar) -> cont 65#;
	PT _ (TI happy_dollar_dollar) -> cont 66#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 67# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pInstr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap22 x') = happyOut22 x} in x'))

pExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap17 x') = happyOut17 x} in x'))

pStmt tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap28 x') = happyOut28 x} in x'))

pDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap24 x') = happyOut24 x} in x'))

pParams tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap20 x') = happyOut20 x} in x'))

pArgs tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap19 x') = happyOut19 x} in x'))

pLambda tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap21 x') = happyOut21 x} in x'))

pSType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap14 x') = happyOut14 x} in x'))

pFType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap15 x') = happyOut15 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
