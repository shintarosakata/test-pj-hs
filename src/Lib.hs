{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type Api = "users" :> Get '[JSON] [User]
      :<|> "user"  :> Capture "id" Int :> Get '[JSON] (Maybe User)

startApp :: Int -> IO ()
startApp port = run port app

app :: Application
app = serve api server

api :: Proxy Api
api = Proxy

server :: Server Api
server = getUsers
    :<|> getUser

getUsers :: Handler [User]
getUsers = pure users

getUser :: Int -> Handler (Maybe User)
getUser id = pure $ findUser id users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "しんたろう" "坂田"
        ]

findUser :: Int -> [User] -> Maybe User
findUser _ []                     = Nothing
findUser n (r:rs) | userId r == n = Just r
                  | otherwise     = findUser n rs
