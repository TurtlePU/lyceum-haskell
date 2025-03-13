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