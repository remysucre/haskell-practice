import Data.List

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x y) = x : (streamToList y)

instance Show a => Show (Stream a) where
    show x = show (take 20 $ streamToList x)

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x y) = Stream (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0,1 ..])
