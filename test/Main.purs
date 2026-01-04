module Test.Main where

import Prelude

import Effect (Effect)

import Data.Text.Diff
  ( DiffLine(..), Limit(..), Comparator(..), ComparisonResult(..), Whitespace(..)
  , compareBy, compareBy_, compareByWP, compareByLines, compareMany
  , lineByLineComparison, onlyDiffsComparison, twoStacksComparison
  )

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

    describe "compareBy" do
      it "returns ThingsEqual for identical strings" do
        compareBy (Zip NoLimit) "hello" "hello"
          `shouldEqual` ThingsEqual

      it "returns ThingsMismatch with Stack comparator" do
        case compareBy (Stack NoLimit) "hello\nworld" "hello\nthere" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` ".. hello\n>> world\n---------------------------------------------------------------\n.. hello\n<< there"

      it "returns ThingsMismatch with Zip comparator" do
        case compareBy (Zip NoLimit) "hello\nworld" "hello\nthere" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` ".. hello\n>> world\n<< there"

      it "returns ThingsMismatch with OnlyDifferent comparator" do
        case compareBy (OnlyDifferent NoLimit) "hello\nworld" "hello\nthere" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` ">> world\n---------------------------------------------------------------\n<< there"

      it "returns ThingsMismatch with Plain comparator" do
        case compareBy Plain "hello" "world" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` "\"hello\" ≠ \"world\""

      it "returns ThingsMismatch with Silent comparator" do
        case compareBy Silent "hello" "world" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` "x"

    describe "compareByWP" do
      it "highlights whitespace in output" do
        case compareByWP (Zip NoLimit) "hello world" "hello  world" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` ">> hello◦world\n<< hello◦◦world"

      it "highlights tabs in output" do
        case compareByWP (Zip NoLimit) "hello\tworld" "hello  world" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` ">> hello→world\n<< hello◦◦world"

    describe "compareBy_" do
      it "allows explicit whitespace configuration" do
        case compareBy_ HighlightWhitespace (Zip NoLimit) "a b" "a  b" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` ">> a◦b\n<< a◦◦b"

      it "works with NormalOutput whitespace" do
        case compareBy_ NormalOutput (Zip NoLimit) "a b" "a  b" of
          ThingsEqual -> false `shouldEqual` true
          ThingsMismatch { diff } ->
            diff `shouldEqual` ">> a b\n<< a  b"