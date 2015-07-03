import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

jl = Append (Product 210)
            (Append (Product 30)
                    (Single (Product 5) 'y')
                    (Append (Product 6)
                            (Single (Product 2) 'e')
                            (Single (Product 3) 'a')))
            (Single (Product 7) 'h')


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m
