{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module defines our app state type and an alias for its handler monad
module Application where

import           Control.Lens            (makeLenses)
import           Control.Monad.State     (get, gets, put)
import           Snap.Snaplet            (Handler, Snaplet, subSnaplet, with)
import           Snap.Snaplet.Heist      (HasHeist, Heist, heistLens)
import           Snap.Snaplet.JWT        (HasJWTState, JWTState, getJWTState,
                                          putJWTState)
import           Snap.Snaplet.Persistent (HasPersistPool, PersistState,
                                          getPersistPool, persistPool)

data App = App
    { _heist   :: Snaplet (Heist App)
    , _jwt     :: Snaplet JWTState
    , _persist :: Snaplet PersistState
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPersistPool (Handler App App) where
    getPersistPool = with persist $ gets persistPool

instance HasJWTState (Handler App App) where
    getJWTState = with jwt get
    putJWTState = with jwt . put

type AppHandler = Handler App App
