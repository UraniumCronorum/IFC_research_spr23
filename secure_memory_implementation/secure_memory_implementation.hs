
type VarName = String
type UID = Integer


----------------
-- Policies

data LatticePol = Public | Erased | User UID
  deriving (Eq, Show)
data Policy = Lattice LatticePol | Erasure VarName Policy Policy
  deriving (Eq, Show)


-- this may be oversimplified
instance Semigroup Policy where
         (Lattice Public) <> pol = pol
         pol <> (Lattice Public) = pol
         p <> q = if (p==q) then p else Lattice Erased

instance Monoid Policy where
         mempty = (Lattice Public)


----------------
-- Erasable data

data UserSecret a = UserSecret Policy a | Deleted

instance Functor UserSecret where
  fmap f (UserSecret pol val) = UserSecret pol (f val)
  fmap f Deleted = Deleted

instance Applicative UserSecret where
  pure v = UserSecret (Lattice Public) v
  (UserSecret p1 f) <*> (UserSecret p2 v) = UserSecret (p1<>p2) (f v)
  Deleted <*> s = Deleted
  s <*> Deleted = Deleted

instance Monad UserSecret where
  return = pure
  (UserSecret p1 v1) >>= g = let (UserSecret p2 v2) = (g v1) in
                             UserSecret (p1<>p2) v2
  Deleted >>= g = Deleted


----------------
-- Secure Memory

type ErasableMemory a = [(VarName, UserSecret a)]

readSecret :: ErasableMemory a -> VarName -> UserSecret a
readSecret memory var = case (lookup var memory) of
                          Nothing -> Deleted
                          Just x -> x
                          
setSecret :: ErasableMemory a -> VarName -> UserSecret a -> ErasableMemory a
setSecret memory var sec = (var, sec) : [(v, s) | (v,s) <- memory, v /= var]

updateSecretWithFunction :: ErasableMemory a -> VarName -> (a->a) -> ErasableMemory a
updateSecretWithFunction memory var f = setSecret memory var $ fmap f $ readSecret memory var

---------------

updatePolicy :: Policy -> VarName -> Integer -> Policy
updatePolicy (Lattice l) v n = Lattice l
updatePolicy (Erasure v p1 p2) u n
  | (u==v) && (n/=0) = p1 <> p2
  | otherwise = (Erasure v p1 p2)

eraseIfNecessary :: UserSecret Integer -> VarName -> Integer -> UserSecret Integer
eraseIfNecessary (UserSecret pol val) v n = UserSecret (updatePolicy pol v n) val

updateErasableMemory :: ErasableMemory Integer -> VarName -> Integer -> ErasableMemory Integer
updateErasableMemory memory v n = [(y, eraseIfNecessary sec v n) | (y,sec)<-memory]

--may need to iterate that

--updateAndEraseIfNecessary