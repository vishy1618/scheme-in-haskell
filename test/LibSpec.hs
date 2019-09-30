module LibSpec where

import Lib
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
    describe "string parsing" $ do
        it "should parse string" $
            readExpression "\"hello world\"" `shouldBe` "Found string: hello world"

        describe "escaped characters" $ do
            it "should parse quotes" $
                readExpression "\"hello\\\"world\"" `shouldBe` "Found string: hello\"world"

            it "should parse carriage return" $
                readExpression "\"hello\\rworld\"" `shouldBe` "Found string: hello\rworld"

            it "should parse tabs" $
                readExpression "\"hello\\tworld\"" `shouldBe` "Found string: hello\tworld"

    describe "atom parsing" $ do
        it "should start with letter" $
            readExpression "kela" `shouldBe` "Found atom: kela"

        it "should start with symbol" $
            readExpression "$kela" `shouldBe` "Found atom: $kela"

        it "should not start with digit" $
            readExpression "1kela" `shouldNotBe` "Found atom: 1kela"

        it "should allow digits in the rest of the atom" $
            readExpression "kela1" `shouldBe` "Found atom: kela1"

    describe "boolean parsing" $ do
        it "parses boolean literal true" $
            readExpression "#t" `shouldBe` "Found bool: True"

        it "parses boolean literal false" $
            readExpression "#f" `shouldBe` "Found bool: False"

    describe "number parsing" $ do
        it "parses number" $
            readExpression "23" `shouldBe` "Found number: 23"

        it "parses octal number" $
            readExpression "#o23" `shouldBe` "Found number: 19"

        it "parses hex number" $
            readExpression "#x1A" `shouldBe` "Found number: 26"
