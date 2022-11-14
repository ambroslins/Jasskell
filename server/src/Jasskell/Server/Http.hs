module Jasskell.Server.Http
  ( serve,
    Route,
    ResponderT,
    route,
    get,
    post,
    notFound,
    param,
  )
where

import Data.Foldable (foldrM)
import Network.Wai (Application)
import UnliftIO (MonadUnliftIO, withRunInIO)
import Web.Twain (Method, ParsableParam, Request, ResponderM, Response)
import Web.Twain qualified as Twain
import Web.Twain.Types (PathPattern (..), ResponderM (..), RouteAction (..))
import Prelude hiding (get)

newtype Route m = Route {runRoute :: Application -> m Application}

serve :: MonadUnliftIO m => Application -> [Route m] -> m Application
serve = foldrM runRoute

get :: MonadUnliftIO m => PathPattern -> ResponderT m Response -> Route m
get = route (Just "GET")

post :: MonadUnliftIO m => PathPattern -> ResponderT m Response -> Route m
post = route (Just "POST")

route ::
  MonadUnliftIO m =>
  Maybe Method ->
  PathPattern ->
  ResponderT m Response ->
  Route m
route method path act =
  Route $ \app ->
    withRunInIO $ \runInIO ->
      pure . ($ app) $
        Twain.route method path $
          ResponderM (runInIO . runResponderT act) >>= Twain.send

notFound :: Application
notFound = Twain.notFound pass

newtype ResponderT m a = ResponderT
  {runResponderT :: Request -> m (Either RouteAction (a, Request))}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadReader r)
    via (StateT Request (ExceptT RouteAction m))

instance MonadTrans ResponderT where
  lift m = ResponderT $ \r -> pure . (,r) <$> m

liftResponder :: MonadIO m => ResponderM a -> ResponderT m a
liftResponder (ResponderM act) = ResponderT (liftIO . act)

param :: (ParsableParam a, MonadIO m) => Text -> ResponderT m a
param = liftResponder . Twain.param