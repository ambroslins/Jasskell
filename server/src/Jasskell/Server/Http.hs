module Jasskell.Server.Http
  ( serve,
    Route,
    route,
    get,
    post,
    notFound,
    MonadResponder (..),
    param,
  )
where

import Jasskell.Server.App (AppT, Env, hoistEnv, runAppT)
import Lucid (HtmlT)
import Network.Wai (Application, Middleware)
import Web.Twain (Method, ParsableParam, ResponderM, Response)
import Web.Twain qualified as Twain
import Web.Twain.Types (PathPattern (..), matchPath)
import Prelude hiding (get)

newtype Route = Route {runRoute :: Env ResponderM -> Middleware}

serve :: Env IO -> Application -> [Route] -> Application
serve env = foldr (`runRoute` env')
  where
    env' = hoistEnv liftIO env

get :: Text -> AppT ResponderM Response -> Route
get = route (Just "GET")

post :: Text -> AppT ResponderM Response -> Route
post = route (Just "POST")

route :: Maybe Method -> Text -> AppT ResponderM Response -> Route
route method path m = Route $ \env ->
  Twain.route method pathPattern $
    runAppT env m >>= Twain.send
  where
    pathPattern = MatchPath $ matchPath path

notFound :: Application
notFound = Twain.notFound pass

class Monad m => MonadResponder m where
  liftResponder :: ResponderM a -> m a

instance MonadResponder ResponderM where
  liftResponder = id

instance MonadResponder m => MonadResponder (AppT m) where
  liftResponder = lift . liftResponder

instance MonadResponder m => MonadResponder (HtmlT m) where
  liftResponder = lift . liftResponder

param :: (ParsableParam a, MonadResponder m) => Text -> m a
param = liftResponder . Twain.param