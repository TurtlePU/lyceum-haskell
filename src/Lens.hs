{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Lens where

import Maybe (State (..), Writer (Writer, wLog), Identity (Identity))
import Data.Kind (Type)

data Layout -- some def...
data StatusCode = Ok | IOError | NetworkError | Pending -- some def...

layout :: State Layout StatusCode
layout = error "TODO"

data AppState = MkAppState
  { appLayout :: Layout
  , appLog :: [String]
  }

-- makeLenses => ...

mainLoop :: State AppState StatusCode
mainLoop = State \appState ->
    let (status, newLayout) = runState layout (appLayout appState)
     in (status, appState { appLayout = newLayout })

data Lens s t = MkLens
  { lensGet :: s -> t
  , lensSet :: s -> t -> s
  }

appLayoutLens :: Lens AppState Layout
appLayoutLens = MkLens
    { lensGet = appLayout
    , lensSet = \appState newLayout ->
        appState { appLayout = newLayout }
    }

withState :: Lens s t -> State t a -> State s a
withState MkLens {..} (State action) =
    State \s ->
        let (x, t') = action (lensGet s)
         in (x, lensSet s t')

mainLoop' :: State AppState StatusCode
mainLoop' = withState appLayoutLens layout

-----------------------------------------------------------

at :: Int -> Lens [a] a
at n = MkLens
    { lensGet = (!! n)
    , lensSet = \list element ->
        let (init', _:tail') = splitAt n list
         in init' ++ [element] ++ tail'
    }

att :: (Int, Int) -> Lens [[a]] a
att (i, j) = MkLens
    { lensGet = lensGet (at j) . lensGet (at i)
    , lensSet = \mt el ->
        let oldRow = lensGet (at i) mt
            newRow = lensSet (at j) oldRow el
         in lensSet (at i) mt newRow
    }

(...) :: Lens t u -> Lens s t -> Lens s u
MkLens gtu stu ... MkLens gst sst = MkLens
    { lensGet = gtu . gst
    , lensSet = \s u -> sst s (stu (gst s) u)
    }

att' :: (Int, Int) -> Lens [[u]] u
att' (i, j) = at j ... at i

class Category (cat :: Type -> Type -> Type) where
    identity :: cat a a
    (<<<) :: cat b c -> cat a b -> cat a c

instance Category (->) where
    identity = id -- \x -> x
    (<<<) = (.) -- \f g x -> f (g x)

instance Category Lens where
    identity = MkLens
        { lensGet = id
        , lensSet = \_ x -> x
        }
    (<<<) = (...)

collect ::
    Functor f =>
    (Layout -> f Layout) ->
    AppState -> f AppState
collect f s =
    let l = lensGet appLayoutLens s
        l' = f l
     in fmap (lensSet appLayoutLens s) l'

lensGet' :: AppState -> Layout
lensGet' s = wLog $ collect (\l -> Writer l l) s

lensSet' :: AppState -> Layout -> AppState
lensSet' s l =
    let Identity s' = collect (\_ -> Identity l) s
     in s'

type VanLaarhoven s t =
    forall f. Functor f => (t -> f t) -> (s -> f s)

appLaarhoven :: VanLaarhoven AppState Layout
appLaarhoven f s =
    fmap (\l -> MkAppState l (appLog s))
         (f (appLayout s))

-- profunctor optics

compose ::
    VanLaarhoven t u -> VanLaarhoven s t -> VanLaarhoven s u
compose f g = g . f