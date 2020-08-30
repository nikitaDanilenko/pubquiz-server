module General.EitherT.Extra where

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

exceptFromMaybe :: Applicative f => Maybe a -> e -> ExceptT e f a
exceptFromMaybe fa e = ExceptT (pure (maybe (Left e) Right fa))

exceptFromMaybeF :: Functor f => f (Maybe a) -> e -> ExceptT e f a
exceptFromMaybeF fma e = ExceptT (fmap (maybe (Left e) Right) fma)

exceptValueOr :: Monad m => ExceptT e m a -> (e -> m a) -> m a
exceptValueOr except err = runExceptT except >>= either err pure
