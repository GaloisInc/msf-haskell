module MSF.Event.Handler where

import qualified Control.Exception as X
import qualified Data.Map as Map


type Handler a = HandlerRef -> a -> IO ()

type HandlerRef = Int

data HandlerMap a = HandlerMap
  { handlers      :: Map.Map HandlerRef (Handler a)
  , handlerNextId :: HandlerRef
  }

-- | An empty map of handlers.
emptyHandlerMap :: HandlerMap a
emptyHandlerMap  = HandlerMap
  { handlers      = Map.empty
  , handlerNextId = 0
  }

-- | Add a handler to a handler map.
addHandler :: Handler a -> HandlerMap a -> (HandlerRef,HandlerMap a)
addHandler k hs = (ref, hs')
  where
  ref = handlerNextId hs
  hs' = HandlerMap
    { handlers      = Map.insert (handlerNextId hs) k (handlers hs)
    , handlerNextId = handlerNextId hs + 1
    }

-- | Remove a handler from a handler map.
removeHandler :: HandlerRef -> HandlerMap a -> HandlerMap a
removeHandler k hs = hs
  { handlers = Map.delete k (handlers hs)
  }

-- | Run a set of handlers over a value.
dispatchHandlers :: HandlerMap a -> a -> IO ()
dispatchHandlers hs a = mapM_ run (Map.toList (handlers hs))
  where
  run (ref,k) = k ref a `X.catch` handler

  -- swallow exceptions to prevent handlers from killing the enclosing thread
  handler :: X.SomeException -> IO ()
  handler e = do
    putStrLn "Encountered exception, swallowing."
    print e
    return ()
