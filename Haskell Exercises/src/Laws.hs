module Laws(semigroupAssoc, monoidLeftIdentity, monoidRightIdentity, monoidAssoc) where


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidLeftIdentity :: (Eq a, Monoid a)=> a-> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m)=> m-> Bool

monoidRightIdentity a = (a <> mempty) == a