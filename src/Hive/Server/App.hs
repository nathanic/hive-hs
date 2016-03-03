{-
 - TODOS
 -  user management, maybe https://github.com/agrafix/users
 -  auth, JSON Web Token stylee (steal from chatqy)
 -  game interaction (websockets! TChans!)
 -  game state management
 -  backing store
 -  everything else
 -}


module Hive.Server.App
    ( startApp
    ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader         (ask, asks, ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either

import Data.Aeson
import Data.Aeson.TH
import Data.List                    (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe                   (fromJust, fromMaybe)
import Data.Monoid                  ((<>))

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import Hive.Server.Types

type API = "users" :> QueryParam "firstName" String
                   :> QueryParam "lastName" String
                   :> Get '[JSON] [User]
    :<|> "user" :> Capture "userid" Int
                :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User
                 :> Post '[JSON] User

api :: Proxy API
api = Proxy

getUsers :: Maybe String -> Maybe String -> AppM [User]
getUsers mfirst mlast = query <$> readUsers
  where
    query = filter (mmiyc mfirst userFirstName)
            . filter (mmiyc mlast userLastName)
    -- match me if you can
    mmiyc (Just q) field = (q ==) . field
    mmiyc Nothing  _     = const True

getUser :: Int -> AppM User
getUser uid = do
    users <- readUsers
    case find ((== uid) . userId) users of
        Just u  -> return u
        Nothing -> lift $ left err404 { errBody = "Dave's not here, man." }

makeUser :: User -> AppM User
makeUser user = do
    refUsers <- asks userDB
    liftIO $ atomically $ modifyTVar refUsers (user :)
    return user


server :: ServerT API AppM
server = getUsers :<|> getUser :<|> makeUser

readerServer :: Config -> Server API
readerServer cfg = enter transmonadify server
  where
    transmonadify :: AppM :~> EitherT ServantErr IO
    transmonadify = Nat $ \x -> runReaderT x cfg

startApp :: IO ()
startApp = do
    ref <- newTVarIO initialUsers
    run 31337 $
        serve api (readerServer Config { userDB = ref })
  where
    initialUsers =
        [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Agent" "Einstein"
        , User 4 "Fig" "Newton"
        ]


readUsers :: AppM [User]
readUsers = readMVar' =<< asks userDB

readMVar' :: (MonadIO m) => TVar a -> m a
readMVar' = liftIO . readTVarIO
