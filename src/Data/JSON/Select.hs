{-# language OverloadedStrings #-}
module Data.JSON.Select where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Data.JSON
import           Data.List           (foldl')
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NE
import           Data.Text           (Text)
import qualified Data.Text           as T
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

data SelectError
  = SelectError SelectErrorReason Path
  deriving (Eq, Show)

data SelectErrorReason
  = MissingKey Text
  | NonMatch Template Value
  | DuplicateVariable Variable
  deriving (Eq, Show)

select
  :: Template -> Value
  -> Validation (NonEmpty SelectError) Assignment
select = go []
  where
    -- the path is accumulated *in reverse*
    go :: Path -> Template -> Value
       -> Validation (NonEmpty SelectError) Assignment
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
      -> Validation (NonEmpty SelectError) Assignment
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

    wrong
      :: Path -> SelectErrorReason
      -> Validation (NonEmpty SelectError) Assignment
    wrong rpath reason
      = failure $ SelectError reason (reverse rpath)

    wrongs
      :: Path -> NonEmpty SelectErrorReason
      -> Validation (NonEmpty SelectError) Assignment
    wrongs rpath reasons
      = Failure $ fmap (\reason -> SelectError reason (reverse rpath)) reasons

    (>>-)
      :: Validation e x
      -> (x -> Validation e y)
      -> Validation e y
    Validation.Success x >>- f = f x
    Validation.Failure e >>- _ = Validation.Failure e

isVariable
  :: Value -> Maybe Text
isVariable (String name)
  | "{{" `T.isPrefixOf` name
  , "}}" `T.isSuffixOf` name
  = Just $ T.drop 2 (T.dropEnd 2 name)
  | otherwise
  = Nothing
