sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (\x acc -> (:) <$> x <*> acc) (pure [])
