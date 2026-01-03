module Test.Main where

import Prelude

import Data.Text.Diff (DiffLine(..), Limit(..), compareByLines, compareMany, lineByLineComparison, onlyDiffsComparison, twoStacksComparison)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "Data.Text.Diff" do
    describe "compareByLines" do
      it "returns BothEqual for identical lines" do
        compareByLines "hello\nworld" "hello\nworld"
          `shouldEqual` [BothEqual "hello", BothEqual "world"]

      it "returns Different for changed lines" do
        compareByLines "hello\nworld" "hello\nthere"
          `shouldEqual` [BothEqual "hello", Different "world" "there"]

      it "returns NewLeft when left has extra lines" do
        compareByLines "hello\nworld\nextra" "hello\nworld"
          `shouldEqual` [BothEqual "hello", BothEqual "world", NewLeft "extra"]

      it "returns NewRight when right has extra lines" do
        compareByLines "hello\nworld" "hello\nworld\nextra"
          `shouldEqual` [BothEqual "hello", BothEqual "world", NewRight "extra"]

      it "handles empty strings" do
        compareByLines "" ""
          `shouldEqual` [BothEqual ""]

    describe "compareMany" do
      it "compares arrays with equal elements" do
        compareMany [1, 2, 3] [1, 2, 3]
          `shouldEqual` [BothEqual 1, BothEqual 2, BothEqual 3]

      it "compares arrays with different elements" do
        compareMany [1, 2, 3] [1, 2, 4]
          `shouldEqual` [BothEqual 1, BothEqual 2, Different 3 4]

      it "compares arrays with different lengths (left longer)" do
        compareMany [1, 2, 3] [1, 2]
          `shouldEqual` [BothEqual 1, BothEqual 2, NewLeft 3]

      it "compares arrays with different lengths (right longer)" do
        compareMany [1, 2] [1, 2, 3]
          `shouldEqual` [BothEqual 1, BothEqual 2, NewRight 3]

    describe "lineByLineComparison" do
      it "formats comparison with no limit" do
        let result = lineByLineComparison NoLimit "hello\nworld" "hello\nthere"
        result `shouldEqual` ".. hello\n>> world\n<< there"

      it "formats comparison with limit" do
        let result = lineByLineComparison (Limit 2) "a\nb\nc\nd" "a\nb\nc\nd"
        result `shouldEqual` ".. a\n.. b\n... 2 lines more."

      it "handles identical strings without limit message" do
        let result = lineByLineComparison (Limit 10) "hello" "hello"
        result `shouldEqual` ".. hello"

    describe "onlyDiffsComparison" do
      it "shows only different lines" do
        let result = onlyDiffsComparison NoLimit "hello\nworld\nfoo" "hello\nthere\nfoo"
        result `shouldEqual` ">> world\n---------------------------------------------------------------\n<< there"

      it "shows additions on the right" do
        let result = onlyDiffsComparison NoLimit "hello" "hello\nworld"
        result `shouldEqual` "\n---------------------------------------------------------------\n-- world"

    describe "twoStacksComparison" do
      it "shows both sides with equal markers" do
        let result = twoStacksComparison NoLimit "hello\nworld" "hello\nthere"
        result `shouldEqual` ".. hello\n>> world\n---------------------------------------------------------------\n.. hello\n<< there"