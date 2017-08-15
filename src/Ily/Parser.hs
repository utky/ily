{-# OPTIONS_GHC -w #-}
{-# OPTIOINS_GHC -a -g -c #-}
module Ily.Parser where

import Ily.Lexer (Token(..), P, lexer, parseFail)
import Ily.Syntax
  ( SCons(..)
  , Id
  , VId(..)
  , TyVar(..)
  , TyCon(..)
  , Lab(..)
  , StrId(..)
  , Op(..)
  , AtPat(..)
  , PatRow(..)
  , Pat(..)
  , Ty(..)
  , TyRow(..)
  , AtExp(..)
  , ExpRow(..)
  , Exp(..)
  , Match(..)
  , MRule(..)
  , Dec(..)
  , ValBind(..)
  , TypeBind(..)
  , DatBind(..)
  , ConBind(..)
  , ExBind(..)
  )
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t36 t37 t38 t39
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn7 (SCons)
	| HappyAbsSyn8 (VId)
	| HappyAbsSyn10 (TyVar)
	| HappyAbsSyn11 (TyCon)
	| HappyAbsSyn13 (StrId)
	| HappyAbsSyn15 (Lab)
	| HappyAbsSyn16 (Op)
	| HappyAbsSyn17 (AtPat)
	| HappyAbsSyn18 ([PatRow])
	| HappyAbsSyn20 (PatRow)
	| HappyAbsSyn21 (Pat)
	| HappyAbsSyn22 (Ty)
	| HappyAbsSyn24 ([TyRow])
	| HappyAbsSyn26 (TyRow)
	| HappyAbsSyn27 (AtExp)
	| HappyAbsSyn28 ([ExpRow])
	| HappyAbsSyn30 (ExpRow)
	| HappyAbsSyn31 (Exp)
	| HappyAbsSyn32 (Match)
	| HappyAbsSyn33 ([MRule])
	| HappyAbsSyn35 (MRule)
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39

action_0 (40) = happyShift action_5
action_0 (41) = happyShift action_10
action_0 (42) = happyShift action_11
action_0 (7) = happyGoto action_34
action_0 _ = happyFail

action_1 (40) = happyShift action_5
action_1 (41) = happyShift action_10
action_1 (42) = happyShift action_11
action_1 (64) = happyShift action_13
action_1 (75) = happyShift action_31
action_1 (79) = happyShift action_32
action_1 (85) = happyShift action_33
action_1 (7) = happyGoto action_27
action_1 (16) = happyGoto action_28
action_1 (17) = happyGoto action_29
action_1 (21) = happyGoto action_30
action_1 _ = happyReduce_18

action_2 (75) = happyShift action_22
action_2 (79) = happyShift action_23
action_2 (92) = happyShift action_24
action_2 (93) = happyShift action_25
action_2 (94) = happyShift action_26
action_2 (10) = happyGoto action_17
action_2 (12) = happyGoto action_18
action_2 (22) = happyGoto action_19
action_2 (23) = happyGoto action_20
action_2 (36) = happyGoto action_21
action_2 _ = happyFail

action_3 (40) = happyShift action_5
action_3 (41) = happyShift action_10
action_3 (42) = happyShift action_11
action_3 (53) = happyShift action_12
action_3 (64) = happyShift action_13
action_3 (67) = happyShift action_14
action_3 (75) = happyShift action_15
action_3 (79) = happyShift action_16
action_3 (7) = happyGoto action_6
action_3 (16) = happyGoto action_7
action_3 (27) = happyGoto action_8
action_3 (31) = happyGoto action_9
action_3 _ = happyReduce_18

action_4 (40) = happyShift action_5
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 _ = happyReduce_48

action_7 (92) = happyShift action_46
action_7 (93) = happyShift action_47
action_7 (9) = happyGoto action_73
action_7 _ = happyFail

action_8 _ = happyReduce_57

action_9 (40) = happyShift action_5
action_9 (41) = happyShift action_10
action_9 (42) = happyShift action_11
action_9 (55) = happyShift action_71
action_9 (64) = happyShift action_13
action_9 (75) = happyShift action_15
action_9 (79) = happyShift action_16
action_9 (82) = happyShift action_72
action_9 (92) = happyShift action_44
action_9 (95) = happyAccept
action_9 (7) = happyGoto action_6
action_9 (8) = happyGoto action_69
action_9 (16) = happyGoto action_7
action_9 (27) = happyGoto action_70
action_9 _ = happyReduce_18

action_10 _ = happyReduce_5

action_11 _ = happyReduce_6

action_12 (40) = happyShift action_5
action_12 (41) = happyShift action_10
action_12 (42) = happyShift action_11
action_12 (64) = happyShift action_13
action_12 (75) = happyShift action_31
action_12 (79) = happyShift action_32
action_12 (85) = happyShift action_33
action_12 (7) = happyGoto action_27
action_12 (16) = happyGoto action_28
action_12 (17) = happyGoto action_29
action_12 (21) = happyGoto action_64
action_12 (32) = happyGoto action_65
action_12 (33) = happyGoto action_66
action_12 (34) = happyGoto action_67
action_12 (35) = happyGoto action_68
action_12 _ = happyReduce_18

action_13 _ = happyReduce_19

action_14 (40) = happyShift action_5
action_14 (41) = happyShift action_10
action_14 (42) = happyShift action_11
action_14 (53) = happyShift action_12
action_14 (64) = happyShift action_13
action_14 (67) = happyShift action_14
action_14 (75) = happyShift action_15
action_14 (79) = happyShift action_16
action_14 (7) = happyGoto action_6
action_14 (16) = happyGoto action_7
action_14 (27) = happyGoto action_8
action_14 (31) = happyGoto action_63
action_14 _ = happyReduce_18

action_15 (40) = happyShift action_5
action_15 (41) = happyShift action_10
action_15 (42) = happyShift action_11
action_15 (53) = happyShift action_12
action_15 (64) = happyShift action_13
action_15 (67) = happyShift action_14
action_15 (75) = happyShift action_15
action_15 (79) = happyShift action_16
action_15 (7) = happyGoto action_6
action_15 (16) = happyGoto action_7
action_15 (27) = happyGoto action_8
action_15 (31) = happyGoto action_62
action_15 _ = happyReduce_18

action_16 (92) = happyShift action_40
action_16 (15) = happyGoto action_58
action_16 (28) = happyGoto action_59
action_16 (29) = happyGoto action_60
action_16 (30) = happyGoto action_61
action_16 _ = happyReduce_53

action_17 _ = happyReduce_35

action_18 _ = happyReduce_40

action_19 (88) = happyShift action_57
action_19 (92) = happyShift action_24
action_19 (93) = happyShift action_25
action_19 (95) = happyAccept
action_19 (12) = happyGoto action_56
action_19 _ = happyFail

action_20 _ = happyReduce_38

action_21 (92) = happyShift action_24
action_21 (93) = happyShift action_25
action_21 (12) = happyGoto action_55
action_21 _ = happyFail

action_22 (75) = happyShift action_22
action_22 (79) = happyShift action_23
action_22 (92) = happyShift action_24
action_22 (93) = happyShift action_25
action_22 (94) = happyShift action_26
action_22 (10) = happyGoto action_17
action_22 (12) = happyGoto action_18
action_22 (22) = happyGoto action_52
action_22 (23) = happyGoto action_20
action_22 (36) = happyGoto action_21
action_22 (37) = happyGoto action_53
action_22 (38) = happyGoto action_54
action_22 _ = happyReduce_73

action_23 (92) = happyShift action_40
action_23 (15) = happyGoto action_48
action_23 (24) = happyGoto action_49
action_23 (25) = happyGoto action_50
action_23 (26) = happyGoto action_51
action_23 _ = happyReduce_44

action_24 _ = happyReduce_12

action_25 _ = happyReduce_13

action_26 _ = happyReduce_10

action_27 _ = happyReduce_21

action_28 (92) = happyShift action_46
action_28 (93) = happyShift action_47
action_28 (9) = happyGoto action_45
action_28 _ = happyFail

action_29 _ = happyReduce_31

action_30 (82) = happyShift action_43
action_30 (92) = happyShift action_44
action_30 (95) = happyAccept
action_30 (8) = happyGoto action_42
action_30 _ = happyFail

action_31 (40) = happyShift action_5
action_31 (41) = happyShift action_10
action_31 (42) = happyShift action_11
action_31 (64) = happyShift action_13
action_31 (75) = happyShift action_31
action_31 (79) = happyShift action_32
action_31 (85) = happyShift action_33
action_31 (7) = happyGoto action_27
action_31 (16) = happyGoto action_28
action_31 (17) = happyGoto action_29
action_31 (21) = happyGoto action_41
action_31 _ = happyReduce_18

action_32 (84) = happyShift action_39
action_32 (92) = happyShift action_40
action_32 (15) = happyGoto action_35
action_32 (18) = happyGoto action_36
action_32 (19) = happyGoto action_37
action_32 (20) = happyGoto action_38
action_32 _ = happyReduce_26

action_33 _ = happyReduce_20

action_34 (95) = happyAccept
action_34 _ = happyFail

action_35 (87) = happyShift action_98
action_35 _ = happyFail

action_36 (80) = happyShift action_97
action_36 _ = happyFail

action_37 (81) = happyShift action_96
action_37 _ = happyReduce_25

action_38 _ = happyReduce_27

action_39 _ = happyReduce_29

action_40 _ = happyReduce_17

action_41 (76) = happyShift action_95
action_41 (82) = happyShift action_43
action_41 (92) = happyShift action_44
action_41 (8) = happyGoto action_42
action_41 _ = happyFail

action_42 (40) = happyShift action_5
action_42 (41) = happyShift action_10
action_42 (42) = happyShift action_11
action_42 (64) = happyShift action_13
action_42 (75) = happyShift action_31
action_42 (79) = happyShift action_32
action_42 (85) = happyShift action_33
action_42 (7) = happyGoto action_27
action_42 (16) = happyGoto action_28
action_42 (17) = happyGoto action_29
action_42 (21) = happyGoto action_94
action_42 _ = happyReduce_18

action_43 (75) = happyShift action_22
action_43 (79) = happyShift action_23
action_43 (92) = happyShift action_24
action_43 (93) = happyShift action_25
action_43 (94) = happyShift action_26
action_43 (10) = happyGoto action_17
action_43 (12) = happyGoto action_18
action_43 (22) = happyGoto action_93
action_43 (23) = happyGoto action_20
action_43 (36) = happyGoto action_21
action_43 _ = happyFail

action_44 _ = happyReduce_7

action_45 (40) = happyShift action_5
action_45 (41) = happyShift action_10
action_45 (42) = happyShift action_11
action_45 (64) = happyShift action_13
action_45 (75) = happyShift action_31
action_45 (79) = happyShift action_32
action_45 (85) = happyShift action_33
action_45 (92) = happyReduce_22
action_45 (93) = happyReduce_18
action_45 (7) = happyGoto action_27
action_45 (16) = happyGoto action_91
action_45 (17) = happyGoto action_92
action_45 _ = happyReduce_22

action_46 _ = happyReduce_8

action_47 _ = happyReduce_9

action_48 (82) = happyShift action_90
action_48 _ = happyFail

action_49 (80) = happyShift action_89
action_49 _ = happyFail

action_50 (81) = happyShift action_88
action_50 _ = happyReduce_43

action_51 _ = happyReduce_45

action_52 (76) = happyShift action_87
action_52 (88) = happyShift action_57
action_52 (92) = happyShift action_24
action_52 (93) = happyShift action_25
action_52 (12) = happyGoto action_56
action_52 _ = happyReduce_72

action_53 (76) = happyShift action_86
action_53 _ = happyFail

action_54 (81) = happyShift action_85
action_54 (39) = happyGoto action_84
action_54 _ = happyReduce_70

action_55 _ = happyReduce_42

action_56 _ = happyReduce_41

action_57 (75) = happyShift action_22
action_57 (79) = happyShift action_23
action_57 (92) = happyShift action_24
action_57 (93) = happyShift action_25
action_57 (94) = happyShift action_26
action_57 (10) = happyGoto action_17
action_57 (12) = happyGoto action_18
action_57 (22) = happyGoto action_83
action_57 (23) = happyGoto action_20
action_57 (36) = happyGoto action_21
action_57 _ = happyFail

action_58 (87) = happyShift action_82
action_58 _ = happyFail

action_59 (80) = happyShift action_81
action_59 _ = happyFail

action_60 (81) = happyShift action_80
action_60 _ = happyReduce_52

action_61 _ = happyReduce_54

action_62 (40) = happyShift action_5
action_62 (41) = happyShift action_10
action_62 (42) = happyShift action_11
action_62 (55) = happyShift action_71
action_62 (64) = happyShift action_13
action_62 (75) = happyShift action_15
action_62 (76) = happyShift action_79
action_62 (79) = happyShift action_16
action_62 (82) = happyShift action_72
action_62 (92) = happyShift action_44
action_62 (7) = happyGoto action_6
action_62 (8) = happyGoto action_69
action_62 (16) = happyGoto action_7
action_62 (27) = happyGoto action_70
action_62 _ = happyReduce_18

action_63 (40) = happyShift action_5
action_63 (41) = happyShift action_10
action_63 (42) = happyShift action_11
action_63 (55) = happyShift action_71
action_63 (64) = happyShift action_13
action_63 (75) = happyShift action_15
action_63 (79) = happyShift action_16
action_63 (92) = happyShift action_44
action_63 (93) = happyReduce_62
action_63 (7) = happyGoto action_6
action_63 (8) = happyGoto action_69
action_63 (16) = happyGoto action_7
action_63 (27) = happyGoto action_70
action_63 _ = happyReduce_62

action_64 (82) = happyShift action_43
action_64 (89) = happyShift action_78
action_64 (92) = happyShift action_44
action_64 (8) = happyGoto action_42
action_64 _ = happyFail

action_65 _ = happyReduce_63

action_66 _ = happyReduce_64

action_67 (86) = happyShift action_77
action_67 _ = happyReduce_65

action_68 _ = happyReduce_66

action_69 (40) = happyShift action_5
action_69 (41) = happyShift action_10
action_69 (42) = happyShift action_11
action_69 (53) = happyShift action_12
action_69 (64) = happyShift action_13
action_69 (67) = happyShift action_14
action_69 (75) = happyShift action_15
action_69 (79) = happyShift action_16
action_69 (7) = happyGoto action_6
action_69 (16) = happyGoto action_7
action_69 (27) = happyGoto action_8
action_69 (31) = happyGoto action_76
action_69 _ = happyReduce_18

action_70 _ = happyReduce_58

action_71 (40) = happyShift action_5
action_71 (41) = happyShift action_10
action_71 (42) = happyShift action_11
action_71 (64) = happyShift action_13
action_71 (75) = happyShift action_31
action_71 (79) = happyShift action_32
action_71 (85) = happyShift action_33
action_71 (7) = happyGoto action_27
action_71 (16) = happyGoto action_28
action_71 (17) = happyGoto action_29
action_71 (21) = happyGoto action_64
action_71 (32) = happyGoto action_75
action_71 (33) = happyGoto action_66
action_71 (34) = happyGoto action_67
action_71 (35) = happyGoto action_68
action_71 _ = happyReduce_18

action_72 (75) = happyShift action_22
action_72 (79) = happyShift action_23
action_72 (92) = happyShift action_24
action_72 (93) = happyShift action_25
action_72 (94) = happyShift action_26
action_72 (10) = happyGoto action_17
action_72 (12) = happyGoto action_18
action_72 (22) = happyGoto action_74
action_72 (23) = happyGoto action_20
action_72 (36) = happyGoto action_21
action_72 _ = happyFail

action_73 _ = happyReduce_49

action_74 (88) = happyShift action_57
action_74 (92) = happyShift action_24
action_74 (93) = happyShift action_25
action_74 (12) = happyGoto action_56
action_74 _ = happyReduce_60

action_75 _ = happyReduce_61

action_76 (40) = happyShift action_5
action_76 (41) = happyShift action_10
action_76 (42) = happyShift action_11
action_76 (55) = happyShift action_71
action_76 (64) = happyShift action_13
action_76 (75) = happyShift action_15
action_76 (79) = happyShift action_16
action_76 (82) = happyShift action_72
action_76 (92) = happyShift action_44
action_76 (93) = happyReduce_59
action_76 (7) = happyGoto action_6
action_76 (8) = happyGoto action_69
action_76 (16) = happyGoto action_7
action_76 (27) = happyGoto action_70
action_76 _ = happyReduce_59

action_77 (40) = happyShift action_5
action_77 (41) = happyShift action_10
action_77 (42) = happyShift action_11
action_77 (64) = happyShift action_13
action_77 (75) = happyShift action_31
action_77 (79) = happyShift action_32
action_77 (85) = happyShift action_33
action_77 (7) = happyGoto action_27
action_77 (16) = happyGoto action_28
action_77 (17) = happyGoto action_29
action_77 (21) = happyGoto action_64
action_77 (35) = happyGoto action_108
action_77 _ = happyReduce_18

action_78 (40) = happyShift action_5
action_78 (41) = happyShift action_10
action_78 (42) = happyShift action_11
action_78 (53) = happyShift action_12
action_78 (64) = happyShift action_13
action_78 (67) = happyShift action_14
action_78 (75) = happyShift action_15
action_78 (79) = happyShift action_16
action_78 (7) = happyGoto action_6
action_78 (16) = happyGoto action_7
action_78 (27) = happyGoto action_8
action_78 (31) = happyGoto action_107
action_78 _ = happyReduce_18

action_79 _ = happyReduce_51

action_80 (92) = happyShift action_40
action_80 (15) = happyGoto action_58
action_80 (30) = happyGoto action_106
action_80 _ = happyFail

action_81 _ = happyReduce_50

action_82 (40) = happyShift action_5
action_82 (41) = happyShift action_10
action_82 (42) = happyShift action_11
action_82 (53) = happyShift action_12
action_82 (64) = happyShift action_13
action_82 (67) = happyShift action_14
action_82 (75) = happyShift action_15
action_82 (79) = happyShift action_16
action_82 (7) = happyGoto action_6
action_82 (16) = happyGoto action_7
action_82 (27) = happyGoto action_8
action_82 (31) = happyGoto action_105
action_82 _ = happyReduce_18

action_83 (88) = happyShift action_57
action_83 (92) = happyShift action_24
action_83 (93) = happyShift action_25
action_83 (12) = happyGoto action_56
action_83 _ = happyReduce_37

action_84 _ = happyReduce_71

action_85 (75) = happyShift action_22
action_85 (79) = happyShift action_23
action_85 (92) = happyShift action_24
action_85 (93) = happyShift action_25
action_85 (94) = happyShift action_26
action_85 (10) = happyGoto action_17
action_85 (12) = happyGoto action_18
action_85 (22) = happyGoto action_104
action_85 (23) = happyGoto action_20
action_85 (36) = happyGoto action_21
action_85 _ = happyFail

action_86 _ = happyReduce_69

action_87 _ = happyReduce_39

action_88 (92) = happyShift action_40
action_88 (15) = happyGoto action_48
action_88 (26) = happyGoto action_103
action_88 _ = happyFail

action_89 _ = happyReduce_36

action_90 (75) = happyShift action_22
action_90 (79) = happyShift action_23
action_90 (92) = happyShift action_24
action_90 (93) = happyShift action_25
action_90 (94) = happyShift action_26
action_90 (10) = happyGoto action_17
action_90 (12) = happyGoto action_18
action_90 (22) = happyGoto action_102
action_90 (23) = happyGoto action_20
action_90 (36) = happyGoto action_21
action_90 _ = happyFail

action_91 (92) = happyShift action_46
action_91 (93) = happyShift action_47
action_91 (9) = happyGoto action_101
action_91 _ = happyFail

action_92 _ = happyReduce_32

action_93 (88) = happyShift action_57
action_93 (92) = happyShift action_24
action_93 (93) = happyShift action_25
action_93 (12) = happyGoto action_56
action_93 _ = happyReduce_34

action_94 (82) = happyShift action_43
action_94 (92) = happyShift action_44
action_94 (8) = happyGoto action_42
action_94 _ = happyReduce_33

action_95 _ = happyReduce_24

action_96 (84) = happyShift action_39
action_96 (92) = happyShift action_40
action_96 (15) = happyGoto action_35
action_96 (20) = happyGoto action_100
action_96 _ = happyFail

action_97 _ = happyReduce_23

action_98 (40) = happyShift action_5
action_98 (41) = happyShift action_10
action_98 (42) = happyShift action_11
action_98 (64) = happyShift action_13
action_98 (75) = happyShift action_31
action_98 (79) = happyShift action_32
action_98 (85) = happyShift action_33
action_98 (7) = happyGoto action_27
action_98 (16) = happyGoto action_28
action_98 (17) = happyGoto action_29
action_98 (21) = happyGoto action_99
action_98 _ = happyReduce_18

action_99 (82) = happyShift action_43
action_99 (92) = happyShift action_44
action_99 (8) = happyGoto action_42
action_99 _ = happyReduce_30

action_100 _ = happyReduce_28

action_101 _ = happyReduce_22

action_102 (88) = happyShift action_57
action_102 (92) = happyShift action_24
action_102 (93) = happyShift action_25
action_102 (12) = happyGoto action_56
action_102 _ = happyReduce_47

action_103 _ = happyReduce_46

action_104 (88) = happyShift action_57
action_104 (92) = happyShift action_24
action_104 (93) = happyShift action_25
action_104 (12) = happyGoto action_56
action_104 _ = happyReduce_74

action_105 (40) = happyShift action_5
action_105 (41) = happyShift action_10
action_105 (42) = happyShift action_11
action_105 (55) = happyShift action_71
action_105 (64) = happyShift action_13
action_105 (75) = happyShift action_15
action_105 (79) = happyShift action_16
action_105 (82) = happyShift action_72
action_105 (92) = happyShift action_44
action_105 (93) = happyReduce_18
action_105 (7) = happyGoto action_6
action_105 (8) = happyGoto action_69
action_105 (16) = happyGoto action_7
action_105 (27) = happyGoto action_70
action_105 _ = happyReduce_56

action_106 _ = happyReduce_55

action_107 (40) = happyShift action_5
action_107 (41) = happyShift action_10
action_107 (42) = happyShift action_11
action_107 (55) = happyShift action_71
action_107 (64) = happyShift action_13
action_107 (75) = happyShift action_15
action_107 (79) = happyShift action_16
action_107 (92) = happyShift action_44
action_107 (93) = happyReduce_68
action_107 (7) = happyGoto action_6
action_107 (8) = happyGoto action_69
action_107 (16) = happyGoto action_7
action_107 (27) = happyGoto action_70
action_107 _ = happyReduce_68

action_108 _ = happyReduce_67

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn7
		 (SInt happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyTerminal (TStr happy_var_1))
	 =  HappyAbsSyn7
		 (SStr happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyTerminal (TChar happy_var_1))
	 =  HappyAbsSyn7
		 (SChar happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn8
		 (VId happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn8
		 (VId happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyTerminal (TLongId happy_var_1))
	 =  HappyAbsSyn8
		 (makeLong QVId happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyTerminal (TTyV happy_var_1))
	 =  HappyAbsSyn10
		 (TyVar happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn11
		 (TyCon happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  12 happyReduction_12
happyReduction_12 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn11
		 (TyCon happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyTerminal (TLongId happy_var_1))
	 =  HappyAbsSyn11
		 (makeLong QTyCon happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn13
		 (StrId happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  14 happyReduction_15
happyReduction_15 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn13
		 (StrId happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  14 happyReduction_16
happyReduction_16 (HappyTerminal (TLongId happy_var_1))
	 =  HappyAbsSyn13
		 (makeLong QStrId happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  15 happyReduction_17
happyReduction_17 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn15
		 (Lab happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  16 happyReduction_18
happyReduction_18  =  HappyAbsSyn16
		 (Nop
	)

happyReduce_19 = happySpecReduce_1  16 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn16
		 (Op
	)

happyReduce_20 = happySpecReduce_1  17 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn17
		 (PWildcard
	)

happyReduce_21 = happySpecReduce_1  17 happyReduction_21
happyReduction_21 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn17
		 (PCon happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  17 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (PVId happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  17 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (PRec happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  17 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (PPat happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (reverse happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  19 happyReduction_26
happyReduction_26  =  HappyAbsSyn18
		 ([]
	)

happyReduce_27 = happySpecReduce_1  19 happyReduction_27
happyReduction_27 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  19 happyReduction_28
happyReduction_28 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_3 : happy_var_1
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn20
		 (PRWildcard
	)

happyReduce_30 = happySpecReduce_3  20 happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn20
		 (PRow happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  21 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (PAtPat happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  21 happyReduction_32
happyReduction_32 (HappyAbsSyn17  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn21
		 (PCtor happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  21 happyReduction_33
happyReduction_33 (HappyAbsSyn21  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (PInfix happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  21 happyReduction_34
happyReduction_34 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (PTyped happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  22 happyReduction_35
happyReduction_35 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn22
		 (TyTyVar happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  22 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (TyRec happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  22 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (TyFunc happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  22 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (TyParen happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn22
		 (TyTyCon [] happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  23 happyReduction_41
happyReduction_41 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (TyTyCon [happy_var_1] happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  23 happyReduction_42
happyReduction_42 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn22
		 (TyTyCon happy_var_1 happy_var_2
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  24 happyReduction_43
happyReduction_43 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (reverse happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_0  25 happyReduction_44
happyReduction_44  =  HappyAbsSyn24
		 ([]
	)

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  25 happyReduction_46
happyReduction_46 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_3 : happy_var_1
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  26 happyReduction_47
happyReduction_47 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn26
		 (TyRow happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  27 happyReduction_48
happyReduction_48 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn27
		 (ESCon happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  27 happyReduction_49
happyReduction_49 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn27
		 (EVId happy_var_1 happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  27 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (ERec happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  27 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (EParen happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  28 happyReduction_52
happyReduction_52 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (reverse happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  29 happyReduction_53
happyReduction_53  =  HappyAbsSyn28
		 ([]
	)

happyReduce_54 = happySpecReduce_1  29 happyReduction_54
happyReduction_54 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn28
		 ([happy_var_1]
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  29 happyReduction_55
happyReduction_55 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_3 : happy_var_1
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  30 happyReduction_56
happyReduction_56 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn30
		 (ERow happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  31 happyReduction_57
happyReduction_57 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn31
		 (EAtExp happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  31 happyReduction_58
happyReduction_58 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (EApp happy_var_1 happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  31 happyReduction_59
happyReduction_59 (HappyAbsSyn31  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (EInfixApp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  31 happyReduction_60
happyReduction_60 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (ETyped happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  31 happyReduction_61
happyReduction_61 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (EHandle happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  31 happyReduction_62
happyReduction_62 (HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (ERaise happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  31 happyReduction_63
happyReduction_63 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (EFn happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  32 happyReduction_64
happyReduction_64 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (MMRule happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  33 happyReduction_65
happyReduction_65 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (reverse happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  34 happyReduction_66
happyReduction_66 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  34 happyReduction_67
happyReduction_67 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_3 : happy_var_1
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  35 happyReduction_68
happyReduction_68 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn35
		 (MRule happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  36 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn36
		 (happy_var_2
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  37 happyReduction_70
happyReduction_70 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 (reverse happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  38 happyReduction_71
happyReduction_71 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_2 : happy_var_1
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  38 happyReduction_72
happyReduction_72 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn38
		 ([happy_var_1]
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_0  38 happyReduction_73
happyReduction_73  =  HappyAbsSyn38
		 ([]
	)

happyReduce_74 = happySpecReduce_2  39 happyReduction_74
happyReduction_74 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 95 95 tk (HappyState action) sts stk;
	TInt happy_dollar_dollar -> cont 40;
	TStr happy_dollar_dollar -> cont 41;
	TChar happy_dollar_dollar -> cont 42;
	TAbsType -> cont 43;
	TAnd -> cont 44;
	TAndAlso -> cont 45;
	TAs -> cont 46;
	TCase -> cont 47;
	TDataType -> cont 48;
	TDo -> cont 49;
	TElse -> cont 50;
	TEnd -> cont 51;
	TException -> cont 52;
	TFn -> cont 53;
	TFun -> cont 54;
	THandle -> cont 55;
	TIf -> cont 56;
	TIn -> cont 57;
	TInfix -> cont 58;
	TInfixr -> cont 59;
	TLet -> cont 60;
	TLocal -> cont 61;
	TNonfix -> cont 62;
	TOf -> cont 63;
	TOp -> cont 64;
	TOpen -> cont 65;
	TOrElse -> cont 66;
	TRaise -> cont 67;
	TRec -> cont 68;
	TThen -> cont 69;
	TType -> cont 70;
	TVal -> cont 71;
	TWith -> cont 72;
	TWithType -> cont 73;
	TWhile -> cont 74;
	TLParen -> cont 75;
	TRParen -> cont 76;
	TLBracket -> cont 77;
	TRBracket -> cont 78;
	TLBrace -> cont 79;
	TRBrace -> cont 80;
	TComma -> cont 81;
	TColon -> cont 82;
	TSemiColon -> cont 83;
	TRowWild -> cont 84;
	TValWild -> cont 85;
	TBar -> cont 86;
	TEq -> cont 87;
	TArrow -> cont 88;
	TFatArrow -> cont 89;
	TSharp -> cont 90;
	TDot -> cont 91;
	TId happy_dollar_dollar -> cont 92;
	TLongId happy_dollar_dollar -> cont 93;
	TTyV happy_dollar_dollar -> cont 94;
	_ -> happyError' tk
	})

happyError_ 95 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = parseError tk

parseScons = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parsePat = happySomeParser where
  happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

parseType = happySomeParser where
  happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

parseExp = happySomeParser where
  happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


makeLong :: ([String] -> String -> a) -> [String] -> a
makeLong f xs = f (init xs) (last xs)

parseError _ = parseFail "Parse failure"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
# 1 "/nix/store/qy94v105wag3z9rgy1rb34zk0x20lkwj-glibc-2.25-dev/include/stdc-predef.h" 1 3 4

# 17 "/nix/store/qy94v105wag3z9rgy1rb34zk0x20lkwj-glibc-2.25-dev/include/stdc-predef.h" 3 4











































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/nix/store/593fzzg81xpqrwniqfbvm9z1xhjivh8a-ghc-8.0.2/lib/ghc-8.0.2/include/ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ilyaletre/ghc25868_0/ghc_2.h" #-}






















































































































































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
