module Ily.ParserSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import qualified Ily.Lexer as L
import qualified Ily.Parser as P
import qualified Ily.Syntax as S

shouldParse :: (Eq a, Show a) => L.P a -> String -> a -> Expectation
shouldParse p i s = L.run p i `shouldBe` Right s


spec :: Spec
spec = do

  describe "scons" $ do
    it "parses 1 digit integer" $ 
      shouldParse P.parseScons "1"  (S.SInt 1)
    it "parses string" $ 
      shouldParse P.parseScons "\"もじれつ\"" (S.SStr "もじれつ")
    it "parses character" $ 
      shouldParse P.parseScons "#\"字\"" (S.SChar '字')

  describe "type" $ do
    it "parse tyvar" $ 
      shouldParse P.parseType "'a" (S.TyTyVar (S.TyVar "'a"))
    it "parse mono tycon" $ 
      shouldParse P.parseType "int" (S.TyTyCon [] (S.TyCon "int"))
    it "parse single tyvar tycon" $ 
      shouldParse P.parseType "'a option"
        (S.TyTyCon [S.TyTyVar (S.TyVar "'a")] (S.TyCon "option"))
    it "parse single type application tycon" $ 
      shouldParse P.parseType "int option"
        (S.TyTyCon [S.TyTyCon [] (S.TyCon "int")] (S.TyCon "option"))
    it "parse type application with paren tycon" $ 
      shouldParse P.parseType "() option"
        (S.TyTyCon [] (S.TyCon "option"))
    it "parse multiple tyvar tycon" $ 
      shouldParse P.parseType "('k, 'v) map"
        (S.TyTyCon [S.TyTyVar (S.TyVar "'k")
                   , S.TyTyVar (S.TyVar "'v")] (S.TyCon "map"))
    it "parse multiple type applicatoin tycon" $ 
      shouldParse P.parseType "(int, string) map"
        (S.TyTyCon [S.TyTyCon [] (S.TyCon "int")
                   , S.TyTyCon [] (S.TyCon "string")] (S.TyCon "map"))
    it "parse ty func" $ 
      shouldParse P.parseType "int -> bool"
        (S.TyFunc (S.TyTyCon [] (S.TyCon "int"))
                  (S.TyTyCon [] (S.TyCon "bool")))
    it "parse tyvar func" $ 
      shouldParse P.parseType "'a -> 'a"
        (S.TyFunc (S.TyTyVar (S.TyVar "'a"))
                  (S.TyTyVar (S.TyVar "'a")))
    it "parse tyrows" $ 
      shouldParse P.parseType "{ one : int, two : bool }"
        (S.TyRec
          [ S.TyRow (S.Lab "one")
                    (S.TyTyCon [] (S.TyCon "int"))
          , S.TyRow (S.Lab "two")
                    (S.TyTyCon [] (S.TyCon "bool"))
          ])

    it "parse type of 2 column tuple" $
      shouldParse P.parseType "int * bool"
        (S.TyTuple
          [ S.TyTyCon [] (S.TyCon "int")
          , S.TyTyCon [] (S.TyCon "bool")
          ])

    it "parse type of 3 column tuple" $
      shouldParse P.parseType "int * bool * string"
        (S.TyTuple
          [ S.TyTyCon [] (S.TyCon "int")
          , S.TyTyCon [] (S.TyCon "bool")
          , S.TyTyCon [] (S.TyCon "string")
          ])

  describe "pat" $ do
    it "atpat _" $ 
      shouldParse P.parsePat "_" 
        (S.PAtPat S.PWildcard)
    it "atpat op vid" $ 
      shouldParse P.parsePat "op >>"
        (S.PAtPat (S.PVId S.Op (S.VId ">>")))
    it "atpat nop vid" $ 
      shouldParse P.parsePat "value"
        (S.PAtPat (S.PVId S.Nop (S.VId "value")))
    it "atpat nop longvid" $ 
      shouldParse P.parsePat "Str.value"
        (S.PAtPat (S.PVId S.Nop (S.QVId ["Str"] "value")))
    it "atpat patrow empty" $ 
      shouldParse P.parsePat "{}"
        (S.PAtPat (S.PRec []))
    it "atpat patrow wildcard" $ 
      shouldParse P.parsePat "{...}"
        (S.PAtPat
          (S.PRec [S.PRWildcard]))
    it "atpat patrow" $ 
      shouldParse P.parsePat "{ value = 1 }"
        (S.PAtPat
          (S.PRec
            [S.PRow
              (S.Lab "value")
              (S.PAtPat (S.PCon (S.SInt 1)))]))
    it "atpat patrows" $ 
      shouldParse P.parsePat "{ name = \"name\", age = 20 }"
        (S.PAtPat
          (S.PRec
            [S.PRow
              (S.Lab "name")
              (S.PAtPat (S.PCon (S.SStr "name"))),
             S.PRow
              (S.Lab "age")
              (S.PAtPat (S.PCon (S.SInt 20)))]))

    -- TODO
    it "pat tuple" $ 
      shouldParse P.parsePat "(x, y)"
        (S.PAtPat
          (S.PTuple
            [ S.PAtPat (S.PVId S.Nop (S.VId "x"))
            , S.PAtPat (S.PVId S.Nop (S.VId "y"))
            ]
          ))

    it "pat constructed pattern" $ 
      shouldParse P.parsePat "just x"
        (S.PCtor
          S.Nop
          (S.VId "just")
          (S.PVId S.Nop (S.VId "x"))
        )
    it "pat infixed value construction" $ 
      shouldParse P.parsePat "x + y"
        (S.PInfix
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.VId "+")
          (S.PAtPat (S.PVId S.Nop (S.VId "y")))
        )

    it "pat paren" $ 
      shouldParse P.parsePat "(x)"
        (S.PAtPat
          (S.PPat
            (S.PAtPat (S.PVId S.Nop (S.VId "x")))))

    it "pat no var typed" $ 
      shouldParse P.parsePat "x : t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.TyTyCon [] (S.TyCon "t")))
    it "pat var typed" $ 
      shouldParse P.parsePat "x : 'a t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.TyTyCon  [S.TyTyVar (S.TyVar "'a")] (S.TyCon "t")))
    it "pat func typed" $ 
      shouldParse P.parsePat "f : s -> t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId "f")))
          (S.TyFunc
            (S.TyTyCon [] (S.TyCon "s"))
            (S.TyTyCon [] (S.TyCon "t"))))
    it "pat rec typed" $ 
      shouldParse P.parsePat "x : { l : t }"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.TyRec
            [S.TyRow (S.Lab "l") (S.TyTyCon [] (S.TyCon "t"))]))

  -- Expression
  describe "exp" $ do

    it "exp scons" $ 
      shouldParse P.parseExp "1"
        (S.EAtExp (S.ESCon (S.SInt 1)))

    it "exp vid" $ 
      shouldParse P.parseExp "x"
        (S.EAtExp (S.EVId S.Nop (S.VId "x")))

    it "exp record" $ 
      shouldParse P.parseExp "{ x = 1, y = \"two\" }"
        (S.EAtExp
          (S.ERec
            [ S.ERow
                 (S.Lab "x")
                 (S.EAtExp (S.ESCon (S.SInt 1)))
            , S.ERow
                 (S.Lab "y")
                 (S.EAtExp (S.ESCon (S.SStr "two")))
            ]
          )
        )


    it "exp 2 columns tuple" $ 
      shouldParse P.parseExp "(1, \"two\")"
        (S.EAtExp $ S.ETuple
          [ S.EAtExp $ S.ESCon (S.SInt 1)
          , S.EAtExp $ S.ESCon (S.SStr "two")
          ])

    it "exp zero element list" $
      shouldParse P.parseExp "[]"
        (S.EAtExp $ S.EList [])

    it "exp 1 element list" $
      shouldParse P.parseExp "[ 1 ]"
        (S.EAtExp $ S.EList
          [ S.EAtExp $ S.ESCon (S.SInt 1)
          ])

    it "exp 2 elements list" $
      shouldParse P.parseExp "[ 1, 2 ]"
        (S.EAtExp $ S.EList
          [ S.EAtExp $ S.ESCon (S.SInt 1)
          , S.EAtExp $ S.ESCon (S.SInt 2)
          ])

    it "exp application" $ 
      shouldParse P.parseExp "inc 1"
        (S.EApp
          (S.EAtExp (S.EVId S.Nop (S.VId "inc")))
          (S.ESCon (S.SInt 1))
        )

    it "exp let val in vid" $ 
      shouldParse P.parseExp "let val x = 1 in x end"
        (S.EAtExp
          (S.ELet
            (S.DVal []
              [S.VBind
                 (S.PAtPat (S.PVId S.Nop (S.VId "x")))
                 (S.EAtExp (S.ESCon (S.SInt 1)))])
            (S.EAtExp
              (S.EVId S.Nop (S.VId "x")))))

    it "exp let val in fun app" $ 
      shouldParse P.parseExp "let val x = 1 in f x end"
        (S.EAtExp
          (S.ELet
            (S.DVal []
              [S.VBind
                 (S.PAtPat (S.PVId S.Nop (S.VId "x")))
                 (S.EAtExp (S.ESCon (S.SInt 1)))])
            (S.EApp
              (S.EAtExp (S.EVId S.Nop (S.VId "f")))
              (S.EVId S.Nop (S.VId "x"))
            )
          )
        )

    it "exp let val in infix app" $ 
      shouldParse P.parseExp "let val x = 1 and y = 2 in x + y end"
        (S.EAtExp
          (S.ELet
            (S.DVal []
              [ S.VBind
                  (S.PAtPat (S.PVId S.Nop (S.VId "x")))
                  (S.EAtExp (S.ESCon (S.SInt 1)))
              , S.VBind
                  (S.PAtPat (S.PVId S.Nop (S.VId "y")))
                  (S.EAtExp (S.ESCon (S.SInt 2)))
                 ])
            (S.EInfixApp
              (S.EVId S.Nop (S.VId "x"))
              (S.VId "+")
              (S.EVId S.Nop (S.VId "y"))
            )
          )
        )

    it "exp infix app" $ 
      shouldParse P.parseExp "1 + 2"
        (S.EInfixApp
          (S.ESCon (S.SInt 1))
          (S.VId "+")
          (S.ESCon (S.SInt 2))
        )

    it "exp infix app with symbol" $ 
      shouldParse P.parseExp "x + y"
        (S.EInfixApp
          (S.EVId S.Nop (S.VId "x"))
          (S.VId "+")
          (S.EVId S.Nop (S.VId "y"))
        )

    it "exp typed" $ 
      shouldParse P.parseExp "1: int"
        (S.ETyped
          (S.EAtExp (S.ESCon (S.SInt 1)))
          (S.TyTyCon [] (S.TyCon "int")))

    -- TODO: handle
    -- TODO: raise
    
    it "exp fn" $ 
      shouldParse P.parseExp "fn x => x + 1"
        (S.EFn
          (S.MMRule
            [S.MRule
               (S.PAtPat (S.PVId S.Nop (S.VId "x")))
               (S.EInfixApp
                 (S.EVId S.Nop (S.VId "x"))
                 (S.VId "+")
                 (S.ESCon (S.SInt 1))
               )
            ]))

  -- Declaration
  describe "dec" $ do

    it "dec int value" $ 
      shouldParse P.parseDec "val i = 1"
        (S.DVal []
          [S.VBind
             (S.PAtPat (S.PVId S.Nop (S.VId "i")))
             (S.EAtExp (S.ESCon (S.SInt 1)))])

    it "dec type alias" $ 
      shouldParse P.parseDec "type i = int"
        (S.DType
          [ S.TBind []
              (S.TyCon "i") 
              (S.TyTyCon [] (S.TyCon "int"))])

    it "dec type alias with tyvar" $ 
      shouldParse P.parseDec "type 'a opt = 'a option"
        (S.DType
          [ S.TBind
              [S.TyVar "'a"]
              (S.TyCon "opt") 
              (S.TyTyCon
                [S.TyTyVar (S.TyVar "'a")]
                (S.TyCon "option"))])

    it "dec datatype" $ 
      shouldParse P.parseDec "datatype bool = true | false"
        (S.DDataType
          [ S.DBind
              []
              (S.TyCon "bool") 
              [ S.CBind
                  S.Nop
                  (S.VId "true")
                  Nothing
              , S.CBind
                  S.Nop
                  (S.VId "false")
                  Nothing
              ]])

    it "dec open one modules" $ 
      shouldParse P.parseDec "open 日本語"
        (S.DOpen
          [ S.StrId "日本語"
          ])

    it "dec open two modules" $ 
      shouldParse P.parseDec "open List 日本語.モジュール"
        (S.DOpen
          [ S.StrId "List"
          , S.QStrId ["日本語"] "モジュール"
          ])

    -- TODO
    it "dec function" $ 
      shouldParse P.parseDec
        (unlines [ "fun not true  = false"
                 , "  | not false = true"
                 ])
        (S.DVal
          []
          [ S.VBind
              (S.PAtPat (S.PVId S.Nop (S.VId "not")))
              (S.EFn
                (S.MMRule
                  [S.MRule
                     (S.PFlatApp [S.PVId S.Nop (S.VId "true")])
                     (S.EFlatApp
                       [ S.EVId S.Nop (S.VId "false")
                       ])
                  ,S.MRule
                     (S.PFlatApp [S.PVId S.Nop (S.VId "false")])
                     (S.EFlatApp
                       [ S.EVId S.Nop (S.VId "true")
                       ])
                  ]))
          ])

  -- Module
  describe "strdec" $ do

    it "strdec" $ 
      shouldParse P.parseTop
        (unlines [ "structure Bool = struct"
                 , "  type i = int"
                 , "end" ])
        (S.TopStr
          (S.Structure
            [S.StrBind (S.StrId "Bool")
              (S.StrBasic
                (S.StrDec
                  [S.DType
                    [ S.TBind []
                        (S.TyCon "i") 
                        (S.TyTyCon [] (S.TyCon "int"))]
                  ]
                )
              )
            ]
          )
        )

    it "strdec dec sequencial" $ 
      shouldParse P.parseTop
        (unlines [ "structure Test = struct"
                 , "  val x = 1"
                 , "  val y = 2"
                 , "end"
                 ])
        (let x = S.DVal []
                   [S.VBind
                      (S.PAtPat (S.PVId S.Nop (S.VId "x")))
                      (S.EAtExp (S.ESCon (S.SInt 1)))]
             y = S.DVal []
                   [S.VBind
                      (S.PAtPat (S.PVId S.Nop (S.VId "y")))
                      (S.EAtExp (S.ESCon (S.SInt 2)))]
         in S.TopStr
              (S.Structure
                [S.StrBind (S.StrId "Test")
                  (S.StrBasic
                    (S.StrDec [x, y])
                  )
                ]
              )
            )
