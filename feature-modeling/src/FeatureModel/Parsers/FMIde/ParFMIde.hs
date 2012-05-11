{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module FeatureModel.Parsers.FMIde.ParFMIde where
import FeatureModel.Parsers.FMIde.AbsFMIde
import FeatureModel.Parsers.FMIde.LexFMIde
import FeatureModel.Parsers.FMIde.ErrM
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.18.2

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = GHC.Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn15 :: (Ident) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Ident)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Grammar) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Grammar)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Production) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Production)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (BaseProd) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (BaseProd)
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (AltProd) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (AltProd)
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (ProdName) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (ProdName)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Term) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Term)
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Option) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Option)
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Expression) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Expression)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([Production]) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([Production])
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ([Term]) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ([Term])
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ([Option]) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ([Option])
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([Expression]) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([Expression])
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x67\x00\x67\x00\x67\x00\x16\x00\x2d\x00\x67\x00\x48\x00\x00\x00\x00\x00\x67\x00\x48\x00\x67\x00\x00\x00\x00\x00\x09\x00\x63\x00\x48\x00\x00\x00\x66\x00\x62\x00\x1b\x00\x47\x00\x2c\x00\x62\x00\x11\x00\x62\x00\x5f\x00\x61\x00\x5e\x00\x5b\x00\x60\x00\x58\x00\x5d\x00\x57\x00\x55\x00\x57\x00\x00\x00\x54\x00\x53\x00\xf5\xff\x55\x00\x00\x00\x00\x00\x5c\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\xf5\xff\xf5\xff\xf5\xff\x00\x00\x51\x00\x46\x00\xf5\xff\x00\x00\x00\x00\x46\x00\x46\x00\x46\x00\x00\x00\x16\x00\x00\x00\x5a\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x2b\x00\x4a\x00\x4f\x00\x3e\x00\x41\x00\x3d\x00\x31\x00\x2a\x00\x52\x00\x50\x00\x18\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x00\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x17\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x26\x00\x1d\x00\x00\x00\x15\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xe0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\xde\xff\xdc\xff\xd9\xff\x00\x00\xf3\xff\xe5\xff\xd8\xff\x00\x00\x00\x00\xe6\xff\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\x00\x00\x00\x00\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\xdc\xff\x00\x00\xd9\xff\xe5\xff\xdf\xff\xf2\xff\x00\x00\x00\x00\xef\xff\xee\xff\xec\xff\xeb\xff\x00\x00\xe8\xff\xe7\xff\x00\x00\x00\x00\x00\x00\xdd\xff\xdc\xff\xe2\xff\xd9\xff\xd7\xff\xda\xff\xe4\xff\xe1\xff\xe3\xff\xe9\xff\x00\x00\xf0\xff\x00\x00\xf1\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x0c\x00\x01\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x06\x00\x02\x00\x03\x00\x04\x00\x02\x00\x08\x00\x08\x00\x08\x00\x0e\x00\x0c\x00\x0c\x00\x0c\x00\x0a\x00\x0b\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x08\x00\x09\x00\x05\x00\x07\x00\x00\x00\x07\x00\x07\x00\x0b\x00\x06\x00\x0b\x00\x0b\x00\x0e\x00\x08\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x10\x00\x01\x00\x0a\x00\x08\x00\x08\x00\x08\x00\x00\x00\x08\x00\x06\x00\x09\x00\x00\x00\x0a\x00\x0b\x00\x07\x00\x0d\x00\x00\x00\x0e\x00\x10\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x04\x00\x06\x00\x05\x00\x00\x00\x05\x00\x02\x00\x03\x00\x04\x00\x00\x00\x00\x00\x02\x00\x03\x00\x04\x00\x00\x00\x0a\x00\x0b\x00\x03\x00\x0d\x00\x0c\x00\x0e\x00\x0e\x00\x10\x00\x03\x00\x04\x00\x0a\x00\x09\x00\x02\x00\x07\x00\x02\x00\x0e\x00\x03\x00\xff\xff\x0e\x00\x10\x00\x04\x00\xff\xff\x05\x00\x10\x00\x10\x00\x0e\x00\x04\x00\xff\xff\xff\xff\x0e\x00\x10\x00\xff\xff\xff\xff\xff\xff\x10\x00\x10\x00\xff\xff\x0e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x12\x00\x43\x00\x0e\x00\x0e\x00\x0e\x00\x29\x00\x1c\x00\x2a\x00\x25\x00\x26\x00\x3c\x00\x0f\x00\x0f\x00\x0f\x00\x0e\x00\x3c\x00\x10\x00\x2b\x00\x36\x00\x37\x00\x12\x00\x38\x00\x12\x00\x12\x00\x34\x00\x35\x00\x1f\x00\x13\x00\x0e\x00\x13\x00\x13\x00\x3d\x00\x1c\x00\x2c\x00\x14\x00\x0e\x00\x3e\x00\x0e\x00\x0e\x00\x0e\x00\x0e\x00\x0e\x00\xff\xff\x27\x00\x2d\x00\x3f\x00\x40\x00\x3a\x00\x12\x00\x17\x00\x1c\x00\x28\x00\x30\x00\x36\x00\x37\x00\x18\x00\x38\x00\x19\x00\x0e\x00\xff\xff\x19\x00\x1f\x00\x1c\x00\x38\x00\x1c\x00\x20\x00\x1a\x00\x44\x00\x23\x00\x1d\x00\x2a\x00\x25\x00\x26\x00\x23\x00\x32\x00\x24\x00\x25\x00\x26\x00\x21\x00\x36\x00\x37\x00\x22\x00\x38\x00\x12\x00\x0e\x00\x0e\x00\xff\xff\x2f\x00\x30\x00\x15\x00\x16\x00\x46\x00\x42\x00\x44\x00\x0e\x00\x2f\x00\x00\x00\x0e\x00\xff\xff\x30\x00\x00\x00\x32\x00\xff\xff\xff\xff\x0e\x00\x3a\x00\x00\x00\x00\x00\x0e\x00\xff\xff\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (12, 40) [
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
	(40 , happyReduce_40)
	]

happy_n_terms = 17 :: Int
happy_n_nonterms = 13 :: Int

happyReduce_12 = happySpecReduce_1  0# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn15
		 (Ident happy_var_1
	)}

happyReduce_13 = happySpecReduce_2  1# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (TGrammar (reverse happy_var_1) happy_var_2
	)}}

happyReduce_14 = happyReduce 5# 2# happyReduction_14
happyReduction_14 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 (TBaseProduction happy_var_1 (reverse happy_var_2) happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_15 = happySpecReduce_3  2# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (TAltProduction happy_var_1 happy_var_2
	)}}

happyReduce_16 = happySpecReduce_2  3# happyReduction_16
happyReduction_16 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (TBaseProd happy_var_1
	)}

happyReduce_17 = happySpecReduce_2  4# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (TAltProd happy_var_1
	)}

happyReduce_18 = happySpecReduce_1  5# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (TProdName happy_var_1
	)}

happyReduce_19 = happySpecReduce_2  5# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (TProdNameL happy_var_2
	)}

happyReduce_20 = happySpecReduce_2  5# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (TProdNameR happy_var_1
	)}

happyReduce_21 = happySpecReduce_1  6# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (TTerm happy_var_1
	)}

happyReduce_22 = happySpecReduce_3  6# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (TOptionalTerm happy_var_2
	)}

happyReduce_23 = happySpecReduce_2  6# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (TOrTerm happy_var_1
	)}

happyReduce_24 = happySpecReduce_2  6# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (TXorTerm happy_var_1
	)}

happyReduce_25 = happySpecReduce_1  7# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (TOption happy_var_1
	)}

happyReduce_26 = happySpecReduce_1  8# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (BasicExp happy_var_1
	)}

happyReduce_27 = happySpecReduce_3  8# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (OrExp happy_var_1 happy_var_3
	)}}

happyReduce_28 = happySpecReduce_3  8# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (AndExp happy_var_1 happy_var_3
	)}}

happyReduce_29 = happySpecReduce_2  8# happyReduction_29
happyReduction_29 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (NotExp happy_var_2
	)}

happyReduce_30 = happySpecReduce_3  8# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (ImpliesExp happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_0  9# happyReduction_31
happyReduction_31  =  happyIn24
		 ([]
	)

happyReduce_32 = happySpecReduce_2  9# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_33 = happySpecReduce_0  10# happyReduction_33
happyReduction_33  =  happyIn25
		 ([]
	)

happyReduce_34 = happySpecReduce_2  10# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_35 = happySpecReduce_0  11# happyReduction_35
happyReduction_35  =  happyIn26
		 ([]
	)

happyReduce_36 = happySpecReduce_1  11# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 ((:[]) happy_var_1
	)}

happyReduce_37 = happySpecReduce_3  11# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_38 = happySpecReduce_0  12# happyReduction_38
happyReduction_38  =  happyIn27
		 ([]
	)

happyReduce_39 = happySpecReduce_1  12# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ((:[]) happy_var_1
	)}

happyReduce_40 = happySpecReduce_3  12# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 16# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS "::") -> cont 1#;
	PT _ (TS ";") -> cont 2#;
	PT _ (TS ":") -> cont 3#;
	PT _ (TS "|") -> cont 4#;
	PT _ (TS "_") -> cont 5#;
	PT _ (TS "[") -> cont 6#;
	PT _ (TS "]") -> cont 7#;
	PT _ (TS "+") -> cont 8#;
	PT _ (TS "*") -> cont 9#;
	PT _ (TS "and") -> cont 10#;
	PT _ (TS "implies") -> cont 11#;
	PT _ (TS "not") -> cont 12#;
	PT _ (TS "or") -> cont 13#;
	PT _ (TV happy_dollar_dollar) -> cont 14#;
	_ -> cont 15#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut16 x))

pProduction tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut17 x))

pBaseProd tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut18 x))

pAltProd tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut19 x))

pProdName tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut20 x))

pTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut21 x))

pOption tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut22 x))

pExpression tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut23 x))

pListProduction tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut24 x))

pListTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut25 x))

pListOption tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut26 x))

pListExpression tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut27 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList





{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
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
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
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
