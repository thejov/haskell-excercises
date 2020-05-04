sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (\x -> (<*>) ((:) <$> x)) (pure [])
