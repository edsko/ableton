module Util.SOP (
    -- * Indices
    Index(..)
  , indices
  , inject
  , indexAll
    -- * Dealing with type-level singleton lists
  , Single
  , SingletonSatisfying
    -- * Metadata for records
  , fieldInfo
    -- * Partial records
  , Partial(..)
  , InvalidValue(..)
  , complete
    -- * Compare records
  , RecordDiff
  , FieldDiff(..)
  , recordDiff
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Data.Kind
import Data.SOP.Dict
import Generics.SOP

{-------------------------------------------------------------------------------
  Indices
-------------------------------------------------------------------------------}

data Index :: [k] -> k -> Type where
  IZ ::               Index (x ': xs) x
  IS :: Index xs x -> Index (y ': xs) x

indices :: forall xs. SListI xs => NP (Index xs) xs
indices = case sList :: SList xs of
            SNil  -> Nil
            SCons -> IZ :* hmap IS indices

inject :: Index xs x -> f x -> NS f xs
inject IZ     = Z
inject (IS i) = S . inject i

indexAll :: All c xs => Index xs x -> Dict c x
indexAll IZ     = Dict
indexAll (IS i) = indexAll i

{-------------------------------------------------------------------------------
  Dealing with type-level singleton lists
-------------------------------------------------------------------------------}

type family Single (xs :: [k]) :: k where
  Single '[x] = x

class (
    xs ~ '[ Single xs ]
  , c (Single xs)
  ) => SingletonSatisfying (c :: k -> Constraint) (xs :: [k])
instance (
    xs ~ '[ Single xs ]
  , c (Single xs)
  ) => SingletonSatisfying (c :: k -> Constraint) (xs :: [k])

{-------------------------------------------------------------------------------
  Metadata for records
-------------------------------------------------------------------------------}

fieldInfo :: HasDatatypeInfo a => Proxy a -> NP FieldInfo (Single (Code a))
fieldInfo p =
    case datatypeInfo p of
      ADT _ _ (Record _ attrs :* Nil) _ -> attrs
      ADT _ _ (Constructor _  :* Nil) _ -> noFields sList
      _otherwise -> error "parseNode: expected record"
  where
    noFields :: SList xs -> NP FieldInfo xs
    noFields SNil  = Nil
    noFields SCons = error "Non-records must be empty"

{-------------------------------------------------------------------------------
  Partial products
-------------------------------------------------------------------------------}

-- | Partial product
newtype Partial (a :: Type) = Partial {
      fromPartial :: NP [] (Single (Code a))
    }

instance SListI (Single (Code a)) => Semigroup (Partial a) where
  Partial a <> Partial b = Partial (hzipWith (++) a b)

instance SListI (Single (Code a)) => Monoid (Partial a) where
  mempty = Partial $ hpure []

data InvalidValue =
    MissingValue FieldName
  | MultipleValues FieldName
  deriving (Show, Exception)

complete :: forall m a req.
     ( SListI (Single (Code a))
     , HasDatatypeInfo a
     , Code a ~ '[ req ]
     , MonadThrow m
     )
  => [Partial a] -> m a
complete =
      fmap (to . SOP . Z)
    . hsequence
    . hzipWith verify (fieldInfo (Proxy @a))
    . fromPartial
    . mconcat
  where
    verify :: FieldInfo x -> [x] -> m x
    verify (FieldInfo n) = \case
        [x] -> return x
        []  -> throwM $ MissingValue   n
        _   -> throwM $ MultipleValues n

{-------------------------------------------------------------------------------
  Diff between two record values
-------------------------------------------------------------------------------}

type RecordDiff a = NP FieldDiff (Single (Code a))

data FieldDiff a = NoDiff | Diff FieldName a a
  deriving (Show)

recordDiff ::
     forall xs a. (Code a ~ '[xs], All Eq xs, HasDatatypeInfo a)
  => a -> a -> RecordDiff a
recordDiff = \x y ->
    hczipWith3
      (Proxy @Eq)
      cmp
      (fieldInfo (Proxy @a))
      (fromSOP (from x))
      (fromSOP (from y))
  where
    fromSOP :: SOP I '[xs] -> NP I xs
    fromSOP (SOP (Z xs)) = xs
    fromSOP (SOP (S xs)) = case xs of {}

    cmp :: Eq x => FieldInfo x -> I x -> I x -> FieldDiff x
    cmp (FieldInfo name) (I x) (I y)
      | x == y    = NoDiff
      | otherwise = Diff name x y
