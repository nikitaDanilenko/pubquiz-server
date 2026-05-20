{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Health where

import           Servant

type HealthApi = "health" :> Verb 'GET 204 '[JSON] NoContent

healthApi :: Proxy HealthApi
healthApi = Proxy

healthServer :: Server HealthApi
healthServer = pure NoContent