module Jasskell.Server.Http
  ( serve,
    Route,
    route,
    get,
    post,
    notFound,
  )
where

import Jasskell.Server.App (AppT, Env, hoistEnv, runAppT)
import Network.Wai (Application, Middleware)
import Web.Twain (Method, ResponderM, Response)
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