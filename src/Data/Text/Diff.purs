-- | A module for comparing and displaying differences between text and other data structures.
-- | Provides various comparison strategies and formatting options for visualizing diffs.
module Data.Text.Diff
    ( class Diffable, toDiffString
    , DiffSide(..)
    , DiffLine(..)
    , Limit(..)
    , Comparator(..)
    , Whitespace(..)
    , ComparisonResult(..)
    , lineByLineComparison
    , onlyDiffsComparison
    , twoStacksComparison
    , lineByLineComparisonWP
    , onlyDiffsComparisonWP
    , twoStacksComparisonWP
    , compareMany
    , compareByLines
    , compareBy
    , compareByWP
    , compareBy_
    ) where

import Prelude

import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Array (catMaybes, take, length) as Array
import Data.Tuple (Tuple)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)
import Data.These (these, These(..))
import Data.Align (class Align, aligned)


-- | A type class for values that can be converted to a string representation
-- | suitable for displaying in diff output.
class Eq a <= Diffable a where
    toDiffString :: a -> String


-- | Represents the state of a single item in a comparison, indicating whether
-- | it's new, changed, equal, or absent in the compared data.
data DiffSide a
    = New a      -- ^ Item exists only in the compared version
    | Changed a  -- ^ Item exists but has been modified
    | Equal a    -- ^ Item exists and is unchanged
    | Absent     -- ^ Item does not exist in this side


-- | Represents a line-by-line comparison result, showing the relationship
-- | between corresponding lines from two sources.
data DiffLine a
    = NewLeft a        -- ^ Line exists only on the left side
    | NewRight a       -- ^ Line exists only on the right side
    | BothEqual a      -- ^ Line is identical on both sides
    | Different a a    -- ^ Line differs between left and right sides


-- | Renders whitespace in output with special highlighting.
data Whitespace
    = HighlightWhitespace
    | NormalOutput


derive instance Eq a => Eq (DiffSide a)
derive instance Eq a => Eq (DiffLine a)
derive instance Functor DiffSide
derive instance Functor DiffLine


type Prefixes =
    { left :: String
    , right :: String
    , eq :: String
    , add :: String
    , sub :: String
    }


prefixes =
    { left : ">>"
    , right : "<<"
    , eq : ".."
    , add : "++"
    , sub : "--"
    } :: Prefixes


-- | Load `DiffLine` value from `These` type
fromThese :: forall a. Eq a => These a a -> DiffLine a
fromThese = case _ of
    This l -> NewLeft l
    That r -> NewRight r
    Both l r ->
        if l == r then BothEqual l
        else Different l r


-- | Give a meaningful resolution of what is on the left side of the diff-line
left :: forall a. DiffLine a -> DiffSide a
left = case _ of
    NewLeft l -> New l
    NewRight _ -> Absent
    BothEqual l -> Equal l
    Different l _ -> Changed l


-- | Give a meaningful resolution of what is on the right side of the diff-line
right :: forall a. DiffLine a -> DiffSide a
right = case _ of
    NewLeft _ -> Absent
    NewRight r -> New r
    BothEqual r -> Equal r
    Different _ r -> Changed r


-- | Specifies a limit on the number of lines to display in comparison output.
data Limit
    = NoLimit    -- ^ Display all lines
    | Limit Int  -- ^ Display at most the specified number of lines


-- | Defines different comparison and formatting strategies
data Comparator
    = Stack Limit           -- ^ Stack format (complete left block on top of the complete right block, separated), with limit
    | Zip Limit             -- ^ Zip format (every other different line zipped, same lines are merged as it is), with limit
    | OnlyDifferent Limit   -- ^ Zip format with only differences, with limit
    | Plain                 -- ^ Plain format (show both values with no special markings if they differ)
    | Silent                -- ^ No output


-- | Performs a line-by-line comparison of two strings, displaying all lines
-- | with prefixes indicating their status (equal, added, removed, or changed).
-- |
-- | Example:
-- |
-- | ```purescript
-- | lineByLineComparison (Limit 10) "hello\nworld" "hello\nthere"
-- | ```
-- |
-- | Renders:
-- |
-- | ```
-- | .. hello
-- | >> world
-- | << there
-- | ```
lineByLineComparison :: Limit -> String -> String -> String
lineByLineComparison = _lineByLineComparison NormalOutput


-- | Same as `lineByLineComparison`, but with whitespace characters highlighted.
lineByLineComparisonWP :: Limit -> String -> String -> String
lineByLineComparisonWP = _lineByLineComparison HighlightWhitespace


_lineByLineComparison :: Whitespace -> Limit -> String -> String -> String
_lineByLineComparison wp limit a b =
    let
        comparedLines = compareByLines a b
        (adjustedLines /\ linesLeft) = _adjustByLimit limit comparedLines
    in
        case linesLeft of
            NoneLeft -> String.joinWith "\n" $ toDiffString <$> map (_awp wp) <$> comparedLines
            LinesLeft n ->
                (String.joinWith "\n" $ toDiffString <$> map (_awp wp) <$> adjustedLines)
                <> "\n... "<> show n <> " lines more."


-- | Compares two strings line-by-line but displays only the lines that differ,
-- | showing them in two separate sections divided by a separator line.
-- |
-- | Example:
-- | ```purescript
-- | onlyDiffsComparison (Limit 10) "hello\nworld" "hello\nthere"
-- | ```
-- |
-- | Renders:
-- |
-- | ```
-- | >> world
-- | ---------------------------------------------------------------
-- | << there
-- | ```
onlyDiffsComparison :: Limit -> String -> String -> String
onlyDiffsComparison = _onlyDiffsComparison NormalOutput


-- | Same as `onlyDiffsComparison`, but with whitespace characters highlighted.
onlyDiffsComparisonWP :: Limit -> String -> String -> String
onlyDiffsComparisonWP = _onlyDiffsComparison HighlightWhitespace


_onlyDiffsComparison :: Whitespace -> Limit -> String -> String -> String
_onlyDiffsComparison wp limit a b =
    let
        comparison = compareByLines a b
        formatLeft = case _ of
            New l     -> Just $ prefixes.add <> " " <> _awp wp l
            Absent    -> Nothing
            Equal _   -> Nothing
            Changed l -> Just $ prefixes.left <> " " <> _awp wp l
        formatRight = case _ of
            New r     -> Just $ prefixes.sub <> " " <> _awp wp r
            Absent    -> Nothing
            Equal _   -> Nothing
            Changed r -> Just $ prefixes.right <> " " <> _awp wp r
        (formattedA /\ leftA) = _adjustByLimit limit $ Array.catMaybes $ formatLeft  <$> left  <$> comparison
        (formattedB /\ leftB) = _adjustByLimit limit $ Array.catMaybes $ formatRight <$> right <$> comparison
    in
           (String.joinWith "\n" formattedA)
        <> "\n---------------------------------------------------------------\n"
        <> (String.joinWith "\n" formattedB)
        <> _linesLeftText leftA leftB


-- | Compares two strings and displays them as two parallel stacks,
-- | showing all lines (equal, added, removed, or changed) in both sections
-- | divided by a separator line.
-- |
-- | Example:
-- | ```purescript
-- | twoStacksComparison (Limit 10) "hello\nworld" "hello\nthere"
-- | ```
-- | Renders:
-- |
-- | ```
-- | .. hello
-- | >> world
-- | ---------------------------------------------------------------
-- | .. hello
-- | << there
-- | ```
twoStacksComparison :: Limit -> String -> String -> String
twoStacksComparison = _twoStacksComparison NormalOutput


-- | Same as `twoStacksComparison`, but with whitespace characters highlighted.
twoStacksComparisonWP :: Limit -> String -> String -> String
twoStacksComparisonWP = _twoStacksComparison HighlightWhitespace


_twoStacksComparison :: Whitespace -> Limit -> String -> String -> String
_twoStacksComparison wp limit a b =
    let
        comparison = compareByLines a b
        formatLeft = case _ of
            New l     -> Just $ prefixes.add   <> " " <> _awp wp l
            Absent    -> Nothing
            Equal l   -> Just $ prefixes.eq    <> " " <> _awp wp l
            Changed l -> Just $ prefixes.left  <> " " <> _awp wp l
        formatRight = case _ of
            New r     -> Just $ prefixes.sub   <> " " <> _awp wp r
            Absent    -> Nothing
            Equal r   -> Just $ prefixes.eq    <> " " <> _awp wp r
            Changed r -> Just $ prefixes.right <> " " <> _awp wp r
        (formattedA /\ leftA) = _adjustByLimit limit $ Array.catMaybes $ formatLeft  <$> left  <$> comparison
        (formattedB /\ leftB) = _adjustByLimit limit $ Array.catMaybes $ formatRight <$> right <$> comparison
    in
           (String.joinWith "\n" $ formattedA)
        <> "\n---------------------------------------------------------------\n"
        <> (String.joinWith "\n" $ formattedB)
        <> _linesLeftText leftA leftB


-- | Compares two alignable structures element-by-element, producing a structure
-- | of `DiffLine` results that indicate the relationship between corresponding elements.
-- |
-- | Example:
-- | ```purescript
-- | compareMany [1, 2, 3] [1, 2, 4]
-- | -- Returns array showing which elements are equal and which differ
-- | ```
compareMany :: forall f a. Eq a => Align f => f a -> f a -> f (DiffLine a)
compareMany as bs = fromThese <$> aligned as bs


data LinesLeft
    = NoneLeft
    | LinesLeft Int


_linesLeftText :: LinesLeft -> LinesLeft -> String
_linesLeftText NoneLeft NoneLeft = ""
_linesLeftText (LinesLeft n) NoneLeft | n <= 0 = ""
_linesLeftText NoneLeft (LinesLeft n) | n <= 0 = ""
_linesLeftText (LinesLeft n) NoneLeft = "\n..." <> show n <> " lines more on the left side."
_linesLeftText NoneLeft (LinesLeft n) = "\n..." <> show n <> " lines more on the right side."
_linesLeftText (LinesLeft nA) (LinesLeft nB) =
    if (nA > 0) || (nB > 0) then
        if (nA == nB)
            then "\n..." <> show nA <> " lines more on both sides."
            else
                "\n..." <> show nA <> " lines more on the left side" <>
                "\n..." <> show nB <> " lines more on the right side."
    else
        ""


_adjustByLimit :: forall a. Limit -> Array a -> (Array a /\ LinesLeft)
_adjustByLimit NoLimit source = source /\ NoneLeft
_adjustByLimit (Limit n) source =
    let
        adjusted = Array.take n source
        linesLeft =  Array.length source - Array.length adjusted
    in
        adjusted /\
            if linesLeft > 0 then LinesLeft linesLeft else NoneLeft


_applyWhitespace :: Whitespace -> String -> String
_applyWhitespace HighlightWhitespace =
    String.replaceAll (String.Pattern " ") (String.Replacement "◦") >>>
    String.replaceAll (String.Pattern "\t") (String.Replacement "→")
_applyWhitespace NormalOutput = identity


-- shorthand for `_applyWhitespace`
_awp = _applyWhitespace :: Whitespace -> String -> String


-- | Splits two strings by newlines and compares them line-by-line,
-- | returning an array of `DiffLine` results.
-- |
-- | Example:
-- | ```purescript
-- | compareByLines "hello\nworld" "hello\nthere"
-- | -- Returns [BothEqual "hello", Different "world" "there"]
-- | ```
compareByLines :: String -> String -> Array (DiffLine String)
compareByLines a b =
    let
        linesA = String.split (String.Pattern "\n") a
        linesB = String.split (String.Pattern "\n") b
    in compareMany linesA linesB


--| Represents the result of a comparison operation, indicating whether
--| the compared items are equal or mismatched, along with the diff output if they are indeed not equal.
data ComparisonResult
    = ThingsEqual
    | ThingsMismatch { diff :: String }


instance Eq ComparisonResult where
    eq ThingsEqual ThingsEqual = true
    eq (ThingsMismatch { diff: d1 }) (ThingsMismatch { diff: d2 }) = d1 == d2 -- if things differ differently, the result is also considered different
    -- eq (ThingsMismatch _) (ThingsMismatch _) = true
    eq _ _ = false


instance Show ComparisonResult where
    show = case _ of
        ThingsEqual -> "=="
        ThingsMismatch { diff } -> "/= { diff: " <> String.take 10 diff <> "... }"


-- | Compares two `Diffable` values using the specified `Comparator`,
-- | returning a `ComparisonResult` that indicates whether they are equal or not,
-- | along with the appropriate diff output if they differ.
compareBy :: forall t. Diffable t => Comparator -> (t -> t -> ComparisonResult)
compareBy = compareBy_ NormalOutput


-- | Compares two `Diffable` values using the specified `Comparator`,
-- | returning a `ComparisonResult` that indicates whether they are equal or not,
-- | along with the appropriate diff output, including whitespace characters, if they differ.
compareByWP :: forall t. Diffable t => Comparator -> (t -> t -> ComparisonResult)
compareByWP = compareBy_ HighlightWhitespace


-- | Compares two `Diffable` values using the specified `Comparator` and `Whitespace` configuration,,
-- | returning a `ComparisonResult` that indicates whether they are equal or not,
-- | along with the appropriate diff output, including whitespace characters, if they differ.
compareBy_ :: forall t. Diffable t => Whitespace -> Comparator -> (t -> t -> ComparisonResult)
compareBy_ wp cmp = \sA sB -> if sA == sB then ThingsEqual else ThingsMismatch { diff: _getDiffyBy wp cmp (toDiffString sA) (toDiffString sB) }


_getDiffyBy :: Whitespace -> Comparator -> (String -> String -> String)
_getDiffyBy NormalOutput (Stack limit)         = twoStacksComparison limit
_getDiffyBy NormalOutput (Zip limit)           = lineByLineComparison limit
_getDiffyBy NormalOutput (OnlyDifferent limit) = onlyDiffsComparison limit
_getDiffyBy NormalOutput Plain                 = \sA sB -> if sA /= sB then show sA <> " ≠ " <> show sB else ""
_getDiffyBy NormalOutput Silent                = \sA sB -> if sA /= sB then "x" else ""
_getDiffyBy HighlightWhitespace (Stack limit)         = twoStacksComparisonWP limit
_getDiffyBy HighlightWhitespace (Zip limit)           = lineByLineComparisonWP limit
_getDiffyBy HighlightWhitespace (OnlyDifferent limit) = onlyDiffsComparisonWP limit
_getDiffyBy HighlightWhitespace Plain                 = \sA sB -> if sA /= sB then show sA <> " ≠ " <> show sB else ""
_getDiffyBy HighlightWhitespace Silent                = \sA sB -> if sA /= sB then "x" else ""


instance Diffable String where
    toDiffString = identity


instance Diffable Int where
    toDiffString = show


instance (Diffable a, Diffable b) => Diffable (These a b) where
    toDiffString = bimap toDiffString toDiffString >>>
        these
            (\lA -> prefixes.add <> " " <> lA)
            (\lB -> prefixes.sub <> " " <> lB)
            (\lA lB ->
                if lA == lB then prefixes.eq <> " " <> lA
                else prefixes.left <> " " <> lA <> "\n" <> prefixes.right <> " " <> lB
            )


instance (Eq a, Diffable a) => Diffable (DiffLine a) where
    toDiffString = map toDiffString >>> case _ of
        NewLeft lA      -> prefixes.add  <> " " <> lA
        NewRight lB     -> prefixes.sub  <> " " <> lB
        BothEqual lA    -> prefixes.eq   <> " " <> lA
        Different lA lB -> prefixes.left <> " " <> lA <> "\n" <> prefixes.right <> " " <> lB


instance (Diffable a, Diffable b) => Diffable (Tuple a b) where
    toDiffString tpl = toDiffString (Tuple.fst tpl) <> "\n" <> toDiffString (Tuple.snd tpl)

instance Diffable a => Diffable (Array a) where
    toDiffString arr = String.joinWith "\n" $ toDiffString <$> arr


instance (Show a) => Show (DiffLine a) where
    show = case _ of
        NewLeft lA      -> "left(" <> show lA <> ")"
        NewRight lB     -> "right(" <> show lB <> ")"
        BothEqual lA    -> "equal(" <> show lA <> ")"
        Different lA lB -> "different(" <> show lA <> ", " <> show lB <> ")"