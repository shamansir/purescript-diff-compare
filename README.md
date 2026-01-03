# purescript-diff-compare

Extension for comparing large amounts of text in PureScript, e.g. in tests

Provides nice output for a large blocks of text, with several ways of configuration.

For example, I use it with `purescript-spec` like this:

```purescript
shouldEqual :: forall m. MonadEffect m => MonadThrow Error m => String -> String -> m Unit
shouldEqual = Diff.compareBy (Diff.OnlyDifferent $ Diff.Limit 20)
```

and use this version of `shouldEqual` when I want to compare large bulks or text. Notice that `compareBy` is from `Diff.Effectful` module, which is made specifically to work in `MonadThrow` environment like `purescript-spec`.

## Examples

### Line by line

Scrolls through every line and outputs them one by one, marks equal lines as `..`, and if it finds a difference between the lines in two samples, it outputs the _left_ line first and then the _right_ line:

```purescript
lineByLineComparison NoLimit "hello\nworld" "hello\nthere"
```

Renders:

```
.. hello
>> world
<< there
```

### Only different lines

Outputs only different lines from two samples as two stacks above each other, first lines from the _left_ and then lines from the _right_:

```purescript
onlyDiffsComparison NoLimit "hello\nworld" "hello\nthere"
```

Renders:

```
>> world
---------------------------------------------------------------
<< there
```

### Compare above / below

Outputs equal and different lines from two samples as two stacks above each other, first lines from the _left_ and then lines from the _right_:

```purescript
twoStacksComparison NoLimit "hello\nworld" "hello\nthere"
```

Renders:

```
.. hello
>> world
---------------------------------------------------------------
.. hello
<< there
```

And yes, instead of `NoLimit` you may configure how many maximum lines you want to have in the output.
