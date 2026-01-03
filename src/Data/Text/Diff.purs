-- | A module for comparing and displaying differences between text and other data structures.
-- | Provides various comparison strategies and formatting options for visualizing diffs.
module Data.Text.Diff
    ( class Diffable, toDiffString
    , DiffSide(..)
    , DiffLine(..)
    , Limit(..)
    , Comparator(..)
    , lineByLineComparison
    , onlyDiffsComparison
    , twoStacksComparison
    , compareMany
    , compareByLines
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


-- | Defines different comparison and formatting strategies (currently not used in exported functions).
data Comparator
    = Stack Limit           -- ^ Stack format with limit
    | Zip Limit             -- ^ Zip format with limit
    | OnlyDifferent Limit   -- ^ Show only differences with limit
    | Plain                 -- ^ Plain format
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
lineByLineComparison limit a b =
    let
        comparedLines = compareByLines a b
        (adjustedLines /\ linesLeft) = _adjustByLimit limit comparedLines
    in
        case linesLeft of
            NoneLeft -> String.joinWith "\n" $ toDiffString <$> comparedLines
            LinesLeft n ->
                (String.joinWith "\n" $ toDiffString <$> adjustedLines)
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
onlyDiffsComparison limit a b =
    let
        comparison = compareByLines a b
        formatLeft = case _ of
            New l     -> Just $ prefixes.add <> " " <> l
            Absent    -> Nothing
            Equal _   -> Nothing
            Changed l -> Just $ prefixes.left <> " " <> l
        formatRight = case _ of
            New r     -> Just $ prefixes.sub <> " " <> r
            Absent    -> Nothing
            Equal _   -> Nothing
            Changed r -> Just $ prefixes.right <> " " <> r
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
twoStacksComparison limit a b =
    let
        comparison = compareByLines a b
        formatLeft = case _ of
            New l     -> Just $ prefixes.add <> " " <> l
            Absent    -> Nothing
            Equal l   -> Just $ prefixes.eq <> " " <> l
            Changed l -> Just $ prefixes.left <> " " <> l
        formatRight = case _ of
            New r     -> Just $ prefixes.sub <> " " <> r
            Absent    -> Nothing
            Equal r   -> Just $ prefixes.eq <> " " <> r
            Changed r -> Just $ prefixes.right <> " " <> r
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