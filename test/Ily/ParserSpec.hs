module Ily.ParserSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
--import qualified Ily.Lexer as L
import qualified Ily.Parser as P
import qualified Ily.Syntax as S

shouldParse :: (Eq a, Show a) => P.Parser a -> String -> a -> Expectation
shouldParse p i e = P.runParser p "none" i `shouldBe` Right e

spec :: Spec
spec = do

  describe "scons" $ do
    it "parses 1 digit integer" $ 
      shouldParse P.scon "1"  (S.SInt 1)
    it "parses string" $ 
      shouldParse P.scon "\"もじれつ\"" (S.SStr "もじれつ")
    it "parses strings" $ 
      shouldParse P.scon "`もじれつ\n二行目`" (S.SStr "もじれつ\n二行目")
    it "parses character" $ 
      shouldParse P.scon "#\"字\"" (S.SChar '字')

  describe "identifier" $ do
    it "op" $ 
      shouldParse P.ope "op" S.Op
    it "alphanum" $ 
      shouldParse P.vid "var123" (S.VId [] "var123")
    it "operator" $ 
      shouldParse P.vid "&&" (S.VId [] "&&")
    it "tyvar" $ 
      shouldParse P.tyvar "'a" (S.TyVar "'a")
    it "tycon" $ 
      shouldParse P.tycon "Some" (S.TyCon [] "Some")
    it "lab" $ 
      shouldParse P.lab "selecor" (S.Lab "selecor")
    it "longvid" $
      shouldParse P.longvid "a.b.c" (S.VId ["a", "b"] "c")
    it "longtycon" $
      shouldParse P.longtycon "A.B.C" (S.TyCon ["A", "B"] "C")

  describe "type" $ do
    it "parse tyvar" $ 
      shouldParse P.ty "'a" (S.TTyVar (S.TyVar "'a"))
    it "parse mono tycon" $ 
      shouldParse P.ty "int" (S.TTyCon [] (S.TyCon [] "int"))
    it "parse single tyvar tycon" $ 
      shouldParse P.ty "'a option"
        (S.TTyCon [S.TTyVar (S.TyVar "'a")] (S.TyCon [] "option"))
    it "parse single type application tycon" $ 
      shouldParse P.ty "int option"
        (S.TTyCon [S.TTyCon [] (S.TyCon [] "int")] (S.TyCon [] "option"))
    it "parse multiple tyvar tycon" $ 
      shouldParse P.ty "('k, 'v) map"
        (S.TTyCon [S.TTyVar (S.TyVar "'k")
                  , S.TTyVar (S.TyVar "'v")] (S.TyCon [] "map"))
    it "parse multiple type applicatoin tycon" $ 
      shouldParse P.ty "(int, string) map"
        (S.TTyCon [S.TTyCon [] (S.TyCon [] "int")
                  , S.TTyCon [] (S.TyCon [] "string")] (S.TyCon [] "map"))
    it "parse ty func" $ 
      shouldParse P.ty "int -> bool"
        (S.TFunc (S.TTyCon [] (S.TyCon [] "int"))
                 (S.TTyCon [] (S.TyCon [] "bool")))
    it "parse tyvar func" $ 
      shouldParse P.ty "'a -> 'a"
        (S.TFunc (S.TTyVar (S.TyVar "'a"))
                 (S.TTyVar (S.TyVar "'a")))
    it "parse tyrows" $ 
      shouldParse P.ty "{ one : int, two : bool }"
        (S.TRec
          [ S.TyRow (S.Lab "one")
                    (S.TTyCon [] (S.TyCon [] "int"))
          , S.TyRow (S.Lab "two")
                    (S.TTyCon [] (S.TyCon [] "bool"))
          ])

    it "parse type of 2 column tuple" $
      shouldParse P.ty "int * bool"
        (S.TTuple
          [ S.TTyCon [] (S.TyCon [] "int")
          , S.TTyCon [] (S.TyCon [] "bool")
          ])

    it "parse type of 3 column tuple" $
      shouldParse P.ty "int * bool * string"
        (S.TTuple
          [ S.TTyCon [] (S.TyCon [] "int")
          , S.TTyCon [] (S.TyCon [] "bool")
          , S.TTyCon [] (S.TyCon [] "string")
          ])

  describe "pat" $ do
    it "atpat _" $ 
      shouldParse P.pat "_" 
        (S.PAtPat S.PWildcard)
    it "atpat op vid" $ 
      shouldParse P.pat "op >>"
        (S.PAtPat (S.PVId S.Op (S.VId [] ">>")))
    it "atpat nop vid" $ 
      shouldParse P.pat "value"
        (S.PAtPat (S.PVId S.Nop (S.VId [] "value")))
    it "atpat nop longvid" $ 
      shouldParse P.pat "Str.value"
        (S.PAtPat (S.PVId S.Nop (S.VId ["Str"] "value")))
    it "atpat patrow empty" $ 
      shouldParse P.pat "{}"
        (S.PAtPat (S.PRec []))
    it "atpat patrow wildcard" $ 
      shouldParse P.pat "{...}"
        (S.PAtPat
          (S.PRec [S.PRWildcard]))
    it "atpat patrow" $ 
      shouldParse P.pat "{ value = 1 }"
        (S.PAtPat
          (S.PRec
            [S.PRow
              (S.Lab "value")
              (S.PAtPat (S.PCon (S.SInt 1)))]))
    it "atpat patrows" $ 
      shouldParse P.pat "{ name = \"name\", age = 20 }"
        (S.PAtPat
          (S.PRec
            [S.PRow
              (S.Lab "name")
              (S.PAtPat (S.PCon (S.SStr "name"))),
             S.PRow
              (S.Lab "age")
              (S.PAtPat (S.PCon (S.SInt 20)))]))

    it "pat tuple" $ 
      shouldParse P.pat "(x, y)"
        (S.PAtPat
          (S.PTuple
            [ S.PAtPat (S.PVId S.Nop (S.VId [] "x"))
            , S.PAtPat (S.PVId S.Nop (S.VId [] "y"))
            ]
          ))

    it "pat unit" $ 
      shouldParse P.pat "()"
        (S.PAtPat
          S.PUnit)

    it "pat constructed pattern" $ 
      shouldParse P.pat "just x"
        (S.PCtor
          S.Nop
          (S.VId [] "just")
          (S.PVId S.Nop (S.VId [] "x"))
        )
    it "pat infixed value construction" $ 
      shouldParse P.pat "x :: xs"
        (S.PInfix
          (S.PVId S.Nop (S.VId [] "x"))
          (S.VId [] "::")
          (S.PVId S.Nop (S.VId [] "xs"))
        )

    it "pat paren" $ 
      shouldParse P.pat "(x)"
        (S.PAtPat
          (S.PParen
            (S.PAtPat (S.PVId S.Nop (S.VId [] "x")))))

    it "pat no var typed" $ 
      shouldParse P.pat "x : t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId [] "x")))
          (S.TTyCon [] (S.TyCon [] "t")))
    it "pat var typed" $ 
      shouldParse P.pat "x : 'a t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId [] "x")))
          (S.TTyCon  [S.TTyVar (S.TyVar "'a")] (S.TyCon [] "t")))
    it "pat func typed" $ 
      shouldParse P.pat "f : s -> t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId [] "f")))
          (S.TFunc
            (S.TTyCon [] (S.TyCon [] "s"))
            (S.TTyCon [] (S.TyCon [] "t"))))
    it "pat rec typed" $ 
      shouldParse P.pat "x : { l : t }"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId [] "x")))
          (S.TRec
            [S.TyRow (S.Lab "l") (S.TTyCon [] (S.TyCon [] "t"))]))

  -- Expression
  describe "exp" $ do

    it "exp scons" $ 
      shouldParse P.exp "1"
        (S.EAtExp (S.ESCon (S.SInt 1)))

    it "exp vid" $ 
      shouldParse P.exp "x"
        (S.EAtExp (S.EVId S.Nop (S.VId [] "x")))

    it "exp record" $ 
      shouldParse P.exp "{ x = 1, y = \"two\" }"
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
      shouldParse P.exp "(1, \"two\")"
        (S.EAtExp $ S.ETuple
          [ S.EAtExp $ S.ESCon (S.SInt 1)
          , S.EAtExp $ S.ESCon (S.SStr "two")
          ])

    it "exp zero element list" $
      shouldParse P.exp "[]"
        (S.EAtExp $ S.EList [])

    it "exp 1 element list" $
      shouldParse P.exp "[ 1 ]"
        (S.EAtExp $ S.EList
          [ S.EAtExp $ S.ESCon (S.SInt 1)
          ])

    it "exp 2 elements list" $
      shouldParse P.exp "[ 1, 2 ]"
        (S.EAtExp $ S.EList
          [ S.EAtExp $ S.ESCon (S.SInt 1)
          , S.EAtExp $ S.ESCon (S.SInt 2)
          ])

    it "exp application" $ 
      shouldParse P.exp "inc 1"
        (S.EApp
          (S.EVId S.Nop (S.VId [] "inc"))
          (S.ESCon (S.SInt 1))
        )

    it "exp application two arguments" $ 
      shouldParse P.exp "add 1 2"
        (S.EApp
          (S.EParen
            (S.EApp
              (S.EVId S.Nop (S.VId [] "add"))
              (S.ESCon (S.SInt 1))))
          (S.ESCon (S.SInt 2)))

    it "exp let val in vid" $ 
      shouldParse P.exp "let val x = 1 in x end"
        (S.EAtExp
          (S.ELet
            [S.DVal []
               [S.VBind
                  (S.PAtPat (S.PVId S.Nop (S.VId [] "x")))
                  (S.EAtExp (S.ESCon (S.SInt 1)))]
            ]
            (S.EAtExp
              (S.EVId S.Nop (S.VId [] "x")))))

    it "exp let val in fun app" $ 
      shouldParse P.exp "let val x = 1 in f x end"
        (S.EAtExp
          (S.ELet
            [S.DVal []
               [S.VBind
                  (S.PAtPat (S.PVId S.Nop (S.VId [] "x")))
                  (S.EAtExp (S.ESCon (S.SInt 1)))]
            ]
            (S.EApp
              (S.EVId S.Nop (S.VId [] "f"))
              (S.EVId S.Nop (S.VId [] "x"))
            )
          )
        )

    it "exp let val in infix app" $ 
      shouldParse P.exp "let val x = 1 and y = 2 in x + y end"
        (S.EAtExp
          (S.ELet
            [S.DVal []
              [ S.VBind
                  (S.PAtPat (S.PVId S.Nop (S.VId [] "x")))
                  (S.EAtExp (S.ESCon (S.SInt 1)))
              , S.VBind
                  (S.PAtPat (S.PVId S.Nop (S.VId [] "y")))
                  (S.EAtExp (S.ESCon (S.SInt 2)))
                 ]
            ]
            (S.EInfixApp
              (S.EVId S.Nop (S.VId [] "x"))
              (S.VId [] "+")
              (S.EVId S.Nop (S.VId [] "y"))
            )
          )
        )

    it "exp infix app" $ 
      shouldParse P.exp "1 + 2"
        (S.EInfixApp
          (S.ESCon (S.SInt 1))
          (S.VId [] "+")
          (S.ESCon (S.SInt 2))
        )

    it "exp infix app with symbol" $ 
      shouldParse P.exp "x + y"
        (S.EInfixApp
          (S.EVId S.Nop (S.VId [] "x"))
          (S.VId [] "+")
          (S.EVId S.Nop (S.VId [] "y"))
        )

    it "exp typed" $ 
      shouldParse P.exp "1: int"
        (S.ETyped
          (S.EAtExp (S.ESCon (S.SInt 1)))
          (S.TTyCon [] (S.TyCon [] "int")))

    -- TODO: handle
    -- TODO: raise
    
    it "exp fn" $ 
      shouldParse P.exp "fn x => inc x"
        (S.EFn
          (S.MMRule
            [S.MRule
               (S.PAtPat (S.PVId S.Nop (S.VId [] "x")))
               (S.EApp
                 (S.EVId S.Nop (S.VId [] "inc"))
                 (S.EVId S.Nop (S.VId [] "x"))
               )
            ]))

  -- Declaration
  describe "dec" $ do

    it "dec int value" $ 
      shouldParse P.dec "val i = 1"
        (S.DVal []
          [S.VBind
             (S.PAtPat (S.PVId S.Nop (S.VId [] "i")))
             (S.EAtExp (S.ESCon (S.SInt 1)))])

    it "dec type alias" $ 
      shouldParse P.dec "type i = int"
        (S.DType
          [ S.TBind []
              (S.TyCon [] "i") 
              (S.TTyCon [] (S.TyCon [] "int"))])

    it "dec type alias with tyvar" $ 
      shouldParse P.dec "type 'a opt = 'a option"
        (S.DType
          [ S.TBind
              [S.TyVar "'a"]
              (S.TyCon [] "opt") 
              (S.TTyCon
                [S.TTyVar (S.TyVar "'a")]
                (S.TyCon [] "option"))])

    it "dec datatype" $ 
      shouldParse P.dec "datatype bool = true | false"
        (S.DDataType
          [ S.DBind
              []
              (S.TyCon [] "bool") 
              [ S.CBind
                  S.Nop
                  (S.VId [] "true")
                  Nothing
              , S.CBind
                  S.Nop
                  (S.VId [] "false")
                  Nothing
              ]])

    it "dec open one modules" $ 
      shouldParse P.dec "open 日本語"
        (S.DOpen
          [ S.StrId [] "日本語"
          ])

    it "dec open two modules" $ 
      shouldParse P.dec "open List 日本語.モジュール"
        (S.DOpen
          [ S.StrId [] "List"
          , S.StrId ["日本語"] "モジュール"
          ])

    it "decs two val declarations" $ 
      shouldParse P.decs
        (unlines [ "val i = `something\nサムシング`"
                 , ""
                 , "val j = 2"
                 ])
        [S.DVal  []
          [S.VBind
             (S.PAtPat (S.PVId S.Nop (S.VId [] "i")))
             (S.EAtExp (S.ESCon (S.SStr "something\nサムシング")))]
        ,S.DVal  []
          [S.VBind
             (S.PAtPat (S.PVId S.Nop (S.VId [] "j")))
             (S.EAtExp (S.ESCon (S.SInt 2)))]
        ]
    -- TODO
--    it "dec function" $ 
--      shouldParse P.dec
--        (unlines [ "fun not true  = false"
--                 , "  | not false = true"
--                 ])
--        (S.DVal
--          []
--          [ S.VBind
--              (S.PAtPat (S.PVId S.Nop (S.VId [] "not")))
--              (S.EFn
--                (S.MMRule
--                  [S.MRule
--                     (S.PFlatApp [S.PVId S.Nop (S.VId [] "true")])
--                     (S.EAtExp
--                       [ S.EVId S.Nop (S.VId "false")
--                       ])
--                  ,S.MRule
--                     (S.PFlatApp [S.PVId S.Nop (S.VId "false")])
--                     (S.EFlatApp
--                       [ S.EVId S.Nop (S.VId "true")
--                       ])
--                  ]))
--          ])

  -- Module
--  describe "strdec" $ do
--
--    it "strdec" $ 
--      shouldParse P.parseTop
--        (unlines [ "structure Bool = struct"
--                 , "  type i = int"
--                 , "end" ])
--        (S.TopStr
--          (S.Structure
--            [S.StrBind (S.StrId "Bool")
--              (S.StrBasic
--                (S.StrDec
--                  [S.DType
--                    [ S.TBind []
--                        (S.TyCon "i") 
--                        (S.TyTyCon [] (S.TyCon "int"))]
--                  ]
--                )
--              )
--            ]
--          )
--        )
--
--    it "strdec dec sequencial" $ 
--      shouldParse P.parseTop
--        (unlines [ "structure Test = struct"
--                 , "  val x = 1"
--                 , "  val y = 2"
--                 , "end"
--                 ])
--        (let x = S.DVal []
--                   [S.VBind
--                      (S.PAtPat (S.PVId S.Nop (S.VId "x")))
--                      (S.EAtExp (S.ESCon (S.SInt 1)))]
--             y = S.DVal []
--                   [S.VBind
--                      (S.PAtPat (S.PVId S.Nop (S.VId "y")))
--                      (S.EAtExp (S.ESCon (S.SInt 2)))]
--         in S.TopStr
--              (S.Structure
--                [S.StrBind (S.StrId "Test")
--                  (S.StrBasic
--                    (S.StrDec [x, y])
--                  )
--                ]
--              )
--            )
