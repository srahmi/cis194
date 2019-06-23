module JoinList
    (
    ) where

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

{--
Append (Product 210)
 (Append (Product 30)
  (Single (Product 5) ’y’)
  (Append (Product 6)
    (Single (Product 2) ’e’)
    (Single (Product 3) ’a’)))
(Single (Product 7) ’h’)
--}
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a `mappend` tag b) a b 
