module Hagato.Vulkan.Builder where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce            (coerce)

newtype Builder s a = Builder { run :: s -> IO (s, a) }

defaultBuilder :: Builder s ()
defaultBuilder = pure ()

instance Functor (Builder s) where
  fmap f (Builder g) =
    Builder $ \s -> do
      (newS, a) <- g s
      pure (newS, f a)

instance Applicative (Builder s) where
  pure a = Builder $ \s -> pure (s, a)
  
  Builder f <*> Builder g =
    Builder $ \s -> do
      (tempS, func) <- f s
      (newS, a) <- g tempS
      pure (newS, func a)

instance Monad (Builder s) where
  Builder f >>= g =
    Builder $ \s -> do
      (newS, a) <- f s
      coerce (g a) newS

instance MonadFail (Builder s) where
  fail = liftIO . fail

instance MonadIO (Builder s) where
  liftIO m =
    Builder $ \s -> do
      a <- m
      pure (s, a)

get :: Builder s s
get = Builder $ \s -> pure (s, s)

put :: s -> Builder s ()
put s = Builder $ \_ -> pure (s, ())

modify :: (s -> s) -> Builder s ()
modify f = do
  s <- get
  put (f s)

runBuilder :: s -> Builder s a -> IO (s, a)
runBuilder = flip coerce

evalBuilder :: s -> Builder s a -> IO a
evalBuilder s (Builder f) =
  fmap snd (f s)

execBuilder :: s -> Builder s a -> IO s
execBuilder s (Builder f) =
  fmap fst (f s)
