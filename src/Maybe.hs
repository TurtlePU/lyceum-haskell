{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}

module Maybe where
import Prelude hiding (Nothing, Just, Maybe)
import Control.Applicative (Applicative (..))

data MaybeInt = Ok Int | NotOk

safeDiv :: Int -> Int -> MaybeInt
safeDiv _ 0 = NotOk
safeDiv x y = Ok (x `div` y)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

data Maybe a = Just a | Nothing
  deriving (Functor, Foldable)

class MyFunctor f where
    myFmap :: (a -> b) -> f a -> f b

maybeFmap :: (a -> b) -> Maybe a -> Maybe b
maybeFmap f (Just x) = Just (f x)
maybeFmap _ Nothing = Nothing

maybeFoldr :: (a -> b -> b) -> b -> Maybe a -> b
maybeFoldr f x (Just y) = f y x
maybeFoldr _ x Nothing = x

class Functor f => MyApplicative f where
    myPure :: a -> f a
    myLiftA2 :: (a -> b -> c) -> f a -> f b -> f c

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just

    liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ Nothing _ = Nothing
    liftA2 _ _ Nothing = Nothing

class Applicative m => MyMonad m where
    myBind :: m a -> (a -> m b) -> m b

instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Just x >>= f = f x
    Nothing >>= _ = Nothing

----------

instance MyFunctor IO where
    myFmap :: (a -> b) -> IO a -> IO b
    myFmap f action = do
        x <- action
        let y = f x
        return y

instance MyApplicative IO where
    myPure :: a -> IO a
    myPure = return

    myLiftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c
    myLiftA2 f actX actY = do
        x <- actX
        y <- actY
        let r = f x y
        return r

instance MyMonad IO where
    myBind :: IO a -> (a -> IO b) -> IO b
    myBind actX cont = do
        x <- actX
        let actY = cont x
        actY


newtype State s a = State { runState :: s -> (a, s) }

-- f s =
--  let (x, s') = g s
--  let (y, s'') = h x s'
--  ...

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State st) = State (\s -> let (a, s') = st s in (f a, s'))

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (\s -> (a, s))

    (<*>) :: State s (a -> b) -> State s a -> State s b
    State sf <*> State sx = State (\s0 ->
        let (f, s1) = sf s0
            (x, s2) = sx s1
         in (f x, s2)
        )

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    State sx >>= k = State (\s0 ->
        let (x, s1)  = sx s0
            State sy = k x
         in sy s1
        )

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s' = State (\_ -> ((), s'))

preincr :: State Int Int -- ++x
preincr = do
    x <- get
    let x' = x + 1
    put x'
    return x'

postincr :: State Int Int -- x++
postincr = do
    x <- get
    let x' = x + 1
    put x'
    return x

newtype Reader e a = Reader { runReader :: e -> a }
-- instance Monad ((->) e)

instance Functor (Reader e) where
    fmap :: (a -> b) -> Reader e a -> Reader e b
    -- (a -> b) -> (e -> a) -> (e -> b) -- (.)
    fmap f (Reader g) = Reader (\e -> f (g e))

instance Applicative (Reader e) where
    pure :: a -> Reader e a
    -- a -> e -> a -- K
    -- Kxy = x
    pure x = Reader (\_ -> x)

    (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
    -- (e -> (a -> b)) -> (e -> a) -> (e -> b) -- S
    -- Sfgx = fx(gx)
    Reader rf <*> Reader rx = Reader (\e -> rf e (rx e))

instance Monad (Reader e) where
    (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    -- (e -> a) -> (a -> e -> b) -> (e -> b)
    Reader rx >>= k = Reader (\e -> runReader (k (rx e)) e)

ask :: Reader e e
ask = Reader (\x -> x) -- id -- I