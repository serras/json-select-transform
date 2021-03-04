{-# language OverloadedStrings #-}
{-# language TupleSections     #-}
{-# language ViewPatterns      #-}
{-|
Description : Matching/selection and transformation of JSON values

'Template's are JSON documents in which some of the elements
are replaced by 'Variable's of the form @{{name}}@. You can then:
- 'select' the value of those variables matching
  against a JSON document.
- 'transform' a template to a new document by injecting the values.
  In this case you can also use @{{#each name}}@ as the single key
  on an object to iterate through all the values in an array.

Inspired by https://selecttransform.github.io/site/.
-}
module Data.JSON.SelectTransform (
  -- * The two functions
  select, transform
  -- * Errors
, Validation(..)
, STError(..), SelectErrorReason(..), TransformErrorReason(..)
, Path, PathElement(..)
  -- * Types involved
, Template, Variable, Assignment
  -- * Simple wrappers for quick testing
, simpleSelect, simpleTransform, simpleTransform'
) where

import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Data.List           (foldl')
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NE
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy      as LazyText
import qualified Data.Vector         as V
import           Validation

-- | Templates are simply Aeson values
--   which have some special keys inside.
type Template = Value

-- | Variables are those elements which
--   get a value when selecting.
type Variable = Text

-- | Results from selecting from a template.
type Assignment = M.HashMap Variable Value

-- | A position within a document.
type Path = [PathElement]

-- | Each of the steps making up a 'Path'.
data PathElement
  = PathElementKey Text
  | PathElementIndex Int
  deriving (Eq, Show)

-- | Error in a JSON value, along with its position in the document.
data STError reason
  = STError reason Path
  -- ^ Regular error during 'select' or 'transform'.
  | DecodeError
  -- ^ Error while decoding in the simple variants.
  deriving (Eq, Show)

-- | Errors during 'select'.
data SelectErrorReason
  = MissingKey Text
  -- ^ A key in the template is not present in the value.
  | NonMatch Template Value
  -- ^ The value does not match the template
  --   (for example, you expected @2@ and the value has @"hello"@).
  | DuplicateVariable Variable
  -- ^ A variable appears more than once in the template.
  deriving (Eq, Show)

-- | Errors during 'transform'.
data TransformErrorReason
  = MissingVariable Variable
  -- ^ A variable is missing from the 'Assignment'.
  | NotAnArray Variable
  -- ^ A value used in @#each@ is not an 'Array'.
  deriving (Eq, Show)

-- | Matches a JSON document against a template,
--   returning the assignment to the variables.
select
  :: Template -> Value
  -> Validation (NonEmpty (STError SelectErrorReason)) Assignment
select = go []
  where
    -- the path is accumulated *in reverse*
    go :: Path -> Template -> Value
       -> Validation (NonEmpty (STError SelectErrorReason)) Assignment
    go _ template value
      | Just var <- isVariable template
      = pure $ M.singleton var value
    go rpath (Object o1) (Object o2)
      = let k1 = M.keysSet o1
            k2 = M.keysSet o2
            missing = S.difference k1 k2
        in if null missing
              then traverse (goO rpath o1 o2) (S.toList k1)
                   >>- foldl' (unionWithNoDuplicates' rpath) ok
              else let problems = map MissingKey $ S.toList missing
                   in wrongs rpath (NE.fromList problems)
    go rpath (Array a1) (Array a2)
      | V.length a1 == V.length a2
      = sequenceA (V.izipWith (\i -> go (PathElementIndex i : rpath)) a1 a2)
        >>- V.foldl' (unionWithNoDuplicates' rpath) ok
    go _ (String t1) (String t2)
      | t1 == t2 = ok
    go _(Number n1) (Number n2)
      | n1 == n2 = ok
    go _ (Bool b1) (Bool b2)
      | b1 == b2 = ok
    go rpath template value
      = wrong rpath (NonMatch template value)

    goO rpath o1 o2 k
      = go (PathElementKey k : rpath) (o1 M.! k) (o2 M.! k)

    unionWithNoDuplicates' rpath acc new =
      acc >>- \old -> unionWithNoDuplicates rpath old new

    unionWithNoDuplicates
      :: Path -> Assignment -> Assignment
      -> Validation (NonEmpty (STError SelectErrorReason)) Assignment
    unionWithNoDuplicates rpath a1 a2
      = let k1 = M.keysSet a1
            k2 = M.keysSet a2
            shared = S.intersection k1 k2
        in if null shared
              then pure $ M.union a1 a2
              else let problems = map DuplicateVariable $ S.toList shared
                   in wrongs rpath (NE.fromList problems)

    ok :: (Semigroup a)
       => Validation a Assignment
    ok = pure M.empty

-- | Transform a template JSON document by injecting
--   the values of some variables.
transform
  :: Assignment -> Template
  -> Validation (NonEmpty (STError TransformErrorReason)) Value
transform assigns = go []
  where
    go :: Path -> Template
       -> Validation (NonEmpty (STError TransformErrorReason)) Value
    go rpath template
      | Just var <- isVariable template
      = case M.lookup var assigns of
          Nothing -> wrong rpath (MissingVariable var)
          Just x  -> pure x
    go rpath (String s)
      = pure $ String (replaceAll s)
    go rpath (Object o)
      | Just (var, inner) <- isEach o
      = case M.lookup var assigns of
          Nothing         -> wrong rpath (MissingVariable var)
          Just x@Array {} -> go rpath x
          Just _          -> wrong rpath (NotAnArray var)
      | otherwise
      = let new_o = M.mapWithKey (\k -> go (PathElementKey k : rpath)) o
        in Object <$> sequenceA new_o
    go rpath (Array a)
      = let new_a = V.imap (\i -> go (PathElementIndex i : rpath)) a
        in Array <$> sequenceA new_a
    go _ primitive  -- numbers, booleans, null
      = pure primitive

    replaceAll s
      = M.foldlWithKey'
           (\acc k v -> T.replace ("{{" <> k <> "}}") (stringify v) acc)
           s assigns

-- | Checks whether a 'Value' represents a 'Variable'.
isVariable
  :: Value -> Maybe Text
isVariable (String name)
  | "{{" `T.isPrefixOf` name
  , "}}" `T.isSuffixOf` name
  = Just $ T.drop 2 (T.dropEnd 2 name)
isVariable other
  = Nothing

-- | Checks whether an 'Object' represent iteration.
isEach
  :: Object
  -> Maybe (Variable, Value)
isEach (M.toList -> [(k, v)])
  | "{{#each " `T.isPrefixOf` k
  , "}}" `T.isSuffixOf` k
  = Just (T.drop 8 (T.dropEnd 2 k), v)
isEach other
  = Nothing

-- | Simple wrapper to test 'select'.
simpleSelect
  :: Text -> Text
  -> Validation (NonEmpty (STError SelectErrorReason)) Assignment
simpleSelect template value
  = case (decodeFromText template, decodeFromText value) of
      (Just template', Just value') -> select template' value'
      _                             -> failure DecodeError

-- | Simple wrapper to test 'select'.
simpleTransform
  :: [(Text,Text)] -> Text
  -> Validation (NonEmpty (STError TransformErrorReason)) Value
simpleTransform assigns template
  = case (mapM (\(k, v) -> (k,) <$> decodeFromText v) assigns, decodeFromText template) of
      (Just assigns', Just template') -> transform (M.fromList assigns') template'
      _                               -> failure DecodeError

-- | Simple wrapper to test 'select'.
simpleTransform'
  :: [(Text,Text)] -> Text
  -> Validation (NonEmpty (STError TransformErrorReason)) Text
simpleTransform' assigns template
  = stringify <$> simpleTransform assigns template

-- Manipulation of 'Text'

decodeFromText
  :: Text -> Maybe Value
decodeFromText = decodeStrict . encodeUtf8

stringify (String s) = s
stringify other      = LazyText.toStrict $ encodeToLazyText other

-- Additional primitives for Validation

wrong
  :: Path -> reason
  -> Validation (NonEmpty (STError reason)) x
wrong rpath reason
  = failure $ STError reason (reverse rpath)

wrongs
  :: Path -> NonEmpty reason
  -> Validation (NonEmpty (STError reason)) x
wrongs rpath reasons
  = Failure $ fmap (\reason -> STError reason (reverse rpath)) reasons

-- | The missing '(>>=)' for 'Validation'.
(>>-)
  :: Validation e x
  -> (x -> Validation e y)
  -> Validation e y
Validation.Success x >>- f = f x
Validation.Failure e >>- _ = Validation.Failure e
