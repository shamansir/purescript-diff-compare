# purescript-diff-compare

Extension for comparing large amounts of text in PureScript, e.g. in tests

Provides nice output for a large blocks of text, with several ways of configuration.

For example, I use it with `purescript-spec` like this:

```purescript
shouldEqual :: forall m. MonadEffect m => MonadThrow Error m => String -> String -> m Unit
shouldEqual = Diff.compareBy (Diff.OnlyDifferent $ Diff.Limit 20)
```

and use this version of `shouldEqual` when I want to compare large bulks or text.