-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the APpy APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Definitions
import Parser
import Transformer

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Smoke tests" [
       testCase "Parser" $ parseSpec str @?= Right ("", eg),
       testCase "Test name1" $ parseSpec str1 @?= Right ("", eg2),
       testCase "Test name2" $ parseSpec str2 @?= Right ("", eg3),
       testCase "Test name3" $ parseSpec str3 @?= Right ("", eg4),
       testCase "Test name4" $ parseSpec str4 @?= Left "Parse error",
       testCase "Test Comment1" $ parseSpec str5 @?= Right ("", eg5),
       testCase "Test Comment2" $ parseSpec str6 @?= Right ("", eg6),
       testCase "Test Comment3" $ parseSpec str7 @?= Right ("", eg7),
       testCase "Test tokLit1" $ parseSpec str8 @?= Right ("", eg8),
       testCase "Test tokLit2" $ parseSpec str9 @?= Right ("", eg9),
       testCase "Test tokLit3" $ parseSpec str10 @?= Left "Parse error",
       testCase "Test CharLit1" $ parseSpec str11 @?= Right ("", eg11),
       testCase "Test CharLit2" $ parseSpec str12 @?= Left "Parse error",
       testCase "Test CharLit3" $ parseSpec str13 @?= Left "Parse error",
       testCase "Test Atom1" $ parseSpec str14 @?= Right ("", eg14),
       testCase "Test Atom2" $ parseSpec str15 @?= Right ("", eg15),
       testCase "Test Atom3" $ parseSpec str16 @?= Right ("", eg16),
       testCase "Test Simple01" $ parseSpec str17 @?= Right ("", eg17),
       testCase "Test Simple02" $ parseSpec str18 @?= Right ("", eg18),
       testCase "Test Simple03" $ parseSpec str19 @?= Right ("", eg19),
       testCase "Test Simple1" $ parseSpec str20 @?= Right ("", eg20),
       testCase "Test Simple2" $ parseSpec str21 @?= Right ("", eg21),
       testCase "Test Simple3" $ parseSpec str22 @?= Right ("", eg22),
       testCase "Test htext1" $ parseSpec str23 @?= Right ("", eg23),
       testCase "Test htext2" $ parseSpec str24 @?= Right ("", eg24),
       testCase "Test htext3" $ parseSpec str25 @?= Left "Parse error",
       testCase "Test htext4" $ parseSpec str26 @?= Left "Parse error",
       testCase "Test LHS1" $ parseSpec str27 @?= Right ("", eg27),
       testCase "Test LHS2" $ parseSpec str28 @?= Right ("", eg28),
       testCase "Test LHS3" $ parseSpec str29 @?= Right ("", eg29),
       testCase "Test Preamble1" $ parseSpec str30 @?= Right ("asdasdasd", eg30),
       testCase "Test Preamble2" $ parseSpec str31 @?= Right ("a s d a s d a s d ", eg31),
       testCase "Test Preamble3" $ parseSpec str32 @?= Right ("a s d a s d a s d ", eg32),
       testCase "Test ERules1" $ parseSpec str33 @?= Right ("", eg33),
       testCase "Test convert" $ convert eg @?= Right g
       ]
       -- convert eg @?= Right g] -- assumes that convert preserves input rule order
       where
       str = "---\n S ::= S \"a\" {_1+1} | \"b\" {0}.\n _ ::= {()}."
       eg = [(("S", RPlain, Nothing),
              EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                            (ESeq [ESimple (SLit "b")] "0")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test name1
       str1 = "---\n S ::= a.\n _ ::= {()}."
       eg2 = [(("S", RPlain, Nothing),
              ESimple (SNTerm "a")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test name2
       str2 = "---\n S ::= asdf1234.\n _ ::= {()}."
       eg3 = [(("S", RPlain, Nothing),
              ESimple (SNTerm "asdf1234")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test name3
       str3 = "---\n S ::= a________1.\n _ ::= {()}."
       eg4 = [(("S", RPlain, Nothing),
              ESimple (SNTerm "a________1")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test name4
       str4 = "---\n S ::= 123.\n _ ::= {()}."
       -- Test Comment1
       str5 = "---\n S --ppp\n::= --ppp\nS \"a\" {_1+1} | \"b\" {0}.\n _ ::= {()}."
       eg5 = [(("S", RPlain, Nothing),
              EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                            (ESeq [ESimple (SLit "b")] "0")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test Comment2
       str6 = "---\n S --ppp\n--ppp\n--ppp\n::= S \"a\" {_1+1} | \"b\" {0}.\n _ ::= {()}."
       eg6 = [(("S", RPlain, Nothing),
              EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                            (ESeq [ESimple (SLit "b")] "0")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test Comment3
       str7 = "---\n S --ppp\n   --ppp\n   --ppp\n::= S \"a\" {_1+1} |  --aqqwe\n \"b\" {0}.\n _ ::= {()}."
       eg7 = [(("S", RPlain, Nothing),
              EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                            (ESeq [ESimple (SLit "b")] "0")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test tokLit1
       str8 = "---\n S ::= \"a\".\n _ ::= {()}."
       eg8 = [(("S", RPlain, Nothing),
              ESimple (SLit "a")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test tokLit2
       str9 = "---\n S ::= \"aa\"a\".\n _ ::= {()}."
       eg9 = [(("S",RPlain,Nothing),ESimple (SLit "aa\"\"a")),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test tokLit3
       str10 = "---\n S ::= \"a\" \"\"\".\n _ ::= {()}."
       -- Test CharLit1
       str11 = "---\n S ::= \'a\'.\n _ ::= {()}."
       eg11 = [(("S",RPlain,Nothing),ESimple (SChar 'a')),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test CharLit2
       str12 = "---\n S ::= \'a\' \'b\'.\n _ ::= {()}."
       -- Test CharLit3
       str13 = "---\n S ::= \'a\'\'.\n _ ::= {()}."
       -- Test Atom1
       str14 = "---\n S ::= a.\n _ ::= {()}."
       eg14 = [(("S",RPlain,Nothing),ESimple (SNTerm "a")),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Atom2
       str15 = "---\n S ::= @.\n _ ::= {()}."
       eg15 = [(("S",RPlain,Nothing),ESimple SAnyChar),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Atom3
       str16 = "---\n S ::= (a).\n _ ::= {()}."
       eg16 = [(("S",RPlain,Nothing),ESimple (SNTerm "a")),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Simple01
       str17 = "---\n S ::= a.\n _ ::= {()}."
       eg17 = [(("S",RPlain,Nothing),ESimple (SNTerm "a")),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Simple02
       str18 = "---\n S ::= @{?aaa}.\n _ ::= {()}."
       eg18 = [(("S",RPlain,Nothing),EPred (ESimple SAnyChar) "aaa"),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Simple03
       str19 = "---\n S ::= @{? isNum }.\n _ ::= {()}."
       eg19 = [(("S",RPlain,Nothing),EPred (ESimple SAnyChar) " isNum "),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Simple1
       str20 = "---\n S ::= a*.\n _ ::= {()}."
       eg20 = [(("S",RPlain,Nothing),EMany (ESimple (SNTerm "a"))),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Simple2
       str21 = "---\n S ::= a?.\n _ ::= {()}."
       eg21 = [(("S",RPlain,Nothing),EOption (ESimple (SNTerm "a"))),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Simple3
       str22 = "---\n S ::= !a.\n _ ::= {()}."
       eg22 = [(("S",RPlain,Nothing),ENot (ESimple (SNTerm "a"))),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test htext1
       str23 = "---\n S ::= a {_1+1}.\n _ ::= {()}."
       eg23 = [(("S",RPlain,Nothing),ESeq [ESimple (SNTerm "a")] "_1+1"),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test htext2
       str24 = "---\n S ::= a {{_1+1}}.\n _ ::= {()}."
       eg24 = [(("S",RPlain,Nothing),ESeq [ESimple (SNTerm "a")] "{_1+1}"),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test htext3
       str25 = "---\n S ::= a {:_1+1} b.\n _ ::= {()}."
       -- Test htext4
       str26 = "---\n S ::= a {?_1+1}.\n _ ::= {()}."
       -- Test LHS1
       str27 = "---\n S {:P}::= a.\n _ ::= {()}."
       eg27 = [(("S",RPlain,Just (AUser "P")),ESimple (SNTerm "a")),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test LHS2
       str28 = "---\n s {:P} ::= a.\n _ ::= {()}."
       eg28 = [(("s",RToken,Just (AUser "P")),ESimple (SNTerm "a")),(("_",RSep,Nothing),ESeq [] "()")]
       -- Test LHS3
       str29 = "---\n _ ::= {()}."
       eg29 = [(("_",RSep,Nothing),ESeq [] "()")]
       -- Test Preamble1
       str30 = "asdasdasd---\n S ::= a.\n _ ::= {()}."
       eg30 = [(("S", RPlain, Nothing),
              ESimple (SNTerm "a")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test Preamble2
       str31 = "a s d a s d a s d ---\n S ::= a.\n _ ::= {()}."
       eg31 = [(("S", RPlain, Nothing),
              ESimple (SNTerm "a")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test Preamble3
       str32 = "a s d a s d a s d ---\n --asas\n S ::= a.\n _ ::= {()}."
       eg32 = [(("S", RPlain, Nothing),
              ESimple (SNTerm "a")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Test ERules1
       str33 = "---\n S ::= S \"a\" {_1+1} | \"b\" {0}.\n _ ::= {()}."
       eg33 = [(("S", RPlain, Nothing),
              EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                            (ESeq [ESimple (SLit "b")] "0")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
       -- Transformer
       -- eg = [(("S", RPlain, Nothing),
       --        EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
       --                      (ESeq [ESimple (SLit "b")] "0")),
       --        (("_", RSep, Nothing), ESeq [] ("()"))]
       g = [(("S", RPlain, Nothing),
          [([SNTerm "S", SLit "a"], AUser "_1+1"),
           ([SLit "b"], AUser "0")]),
         (("_", RSep, Nothing), [([], AUser "()")])]