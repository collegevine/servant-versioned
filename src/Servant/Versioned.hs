{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Versioned where

import Data.Proxy (Proxy(..))
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Media.MediaType ((//))
import Servant

--
--
--

type family Fold (f :: k -> k -> k) (a :: k) (ax :: [k]) where
    Fold f a '[] = a
    Fold f a (b ': rx) = Fold f (f a b) rx

type family AssocAlt a where
    AssocAlt ((a :<|> b) :<|> c) = AssocAlt a :<|> (AssocAlt (b :<|> c))
    AssocAlt a = a

--
--
--

--
--
--

data Exclude

instance HasServer Exclude ctx where
    type ServerT Exclude m = m Void

    route Proxy ctx d = route voidServer ctx d
        where voidServer = Proxy :: Proxy (Verb GET 404 '[OnlyFail] Void)

    hoistServerWithContext _ ctxProxy = hoistServerWithContext voidServer ctxProxy
        where voidServer = Proxy :: Proxy (Verb GET 404 '[OnlyFail] Void)

--
--
--

data OnlyFail

instance MimeRender OnlyFail Void where
    mimeRender _ = const ""

instance Accept OnlyFail where
    contentType = const ("application" // "*")

--
--
--

data Versioned (ax :: [Symbol])
data Versions (ax :: [Symbol])

instance (
    HasServer (AssocAlt (Fold (:<|>) (a :> CompileVersion a api) (CompileVersions api ax))) ctx,
    KnownSymbol a) =>
    HasServer (Versioned (a ': ax) :> api) ctx where
    type ServerT (Versioned (a ': ax) :> api) m =
        ServerT (AssocAlt (Fold (:<|>) (a :> CompileVersion a api) (CompileVersions api ax))) m

    route Proxy ctx d = route versionedServer ctx d
        where
        versionedServer =
            Proxy :: Proxy (AssocAlt (Fold (:<|>) (a :> CompileVersion a api) (CompileVersions api ax)))

    hoistServerWithContext _ ctx = hoistServerWithContext versionedServer ctx
        where
        versionedServer =
            Proxy :: Proxy (AssocAlt (Fold (:<|>) (a :> CompileVersion a api) (CompileVersions api ax)))

instance HasServer api ctx => HasServer (Versions vx :> api) ctx where
    type ServerT (Versions vx :> api) m = ServerT api m
    route Proxy ctx d = route (Proxy :: Proxy api) ctx d
    hoistServerWithContext _ ctx = hoistServerWithContext (Proxy :: Proxy api) ctx

type family CompileVersions (api :: *) (vx :: [Symbol]) where
    CompileVersions api '[] = '[]
    CompileVersions api (v ': vx) = (v :> CompileVersion v api) ': CompileVersions api vx

type family CompileVersion (a :: Symbol) api where
    CompileVersion a (Versions ax :> Exclude) =
        Exclude
    CompileVersion a (Versions (a ': ax) :> api) =
        CompileVersion a api
    CompileVersion a (Versions (b ': ax) :> api) =
        CompileVersion a (Versions ax :> api)
    CompileVersion a (Versions '[] :> api) =
        Exclude
    CompileVersion a (b :<|> c) =
        EraseExclusions (CompileVersion a b :<|> CompileVersion a c)
    CompileVersion a (b :> Exclude) =
        Exclude
    CompileVersion a (b :> c) =
        PropagateExclusion (b :> CompileVersion a c)
    CompileVersion a api =
        api

type family EraseExclusions api where
    EraseExclusions (Exclude :<|> a) = a
    EraseExclusions (a :<|> Exclude) = a
    EraseExclusions a = a

type family PropagateExclusion api where
    PropagateExclusion (a :> Exclude) = Exclude
    PropagateExclusion a = a

--
--
--

class ServerVersion api1 api2 where
    serverVersion ::
        Proxy api1 ->
        Proxy api2 ->
        m Void ->
        ServerT api1 m ->
        ServerT api2 m

instance ServerVersion api Exclude where
    serverVersion _ _ errF _ = errF

instance ServerVersion api api where
    serverVersion _ _ _ srv = srv

instance ServerVersion (api11 :<|> api12) api11 where
    serverVersion _ _ _ (srv1 :<|> _) = srv1

instance ServerVersion (api11 :<|> api12) api12 where
    serverVersion _ _ _ (_ :<|> srv2) = srv2

instance ServerVersion api12 api22 => ServerVersion (api11 :<|> api12) (api11 :<|> api22) where
    serverVersion _ _ errF (srv1 :<|> srv2) = srv1 :<|> serverVersion api12 api22 errF srv2
        where
        api12 = Proxy :: Proxy api12
        api22 = Proxy :: Proxy api22

instance ServerVersion api11 api21 => ServerVersion (api11 :<|> api12) (api21 :<|> api12) where
    serverVersion _ _ errF (srv1 :<|> srv2) = serverVersion api11 api21 errF srv1 :<|> srv2
        where
        api11 = Proxy :: Proxy api11
        api21 = Proxy :: Proxy api21

instance ServerVersion api1 api2 => ServerVersion (Capture c a :> api1) (Capture c a :> api2) where
    serverVersion _ _ errF srv = serverVersion api1 api2 errF . srv
        where
        api1 = Proxy :: Proxy api1
        api2 = Proxy :: Proxy api2

instance ServerVersion api1 api2 => ServerVersion (QueryParam c a :> api1) (QueryParam c a :> api2) where
    serverVersion _ _ errF srv = serverVersion api1 api2 errF . srv
        where
        api1 = Proxy :: Proxy api1
        api2 = Proxy :: Proxy api2

instance ServerVersion api1 api2 => ServerVersion (QueryParams c a :> api1) (QueryParams c a :> api2) where
    serverVersion _ _ errF srv = serverVersion api1 api2 errF . srv
        where
        api1 = Proxy :: Proxy api1
        api2 = Proxy :: Proxy api2

type ConcreteHeader = (Header' '[Optional, Strict] :: Symbol -> * -> *)

instance ServerVersion api1 api2 => ServerVersion (ConcreteHeader c a :> api1) (ConcreteHeader c a :> api2) where
    serverVersion _ _ errF srv = serverVersion api1 api2 errF . srv
        where
        api1 = Proxy :: Proxy api1
        api2 = Proxy :: Proxy api2

--
--
--

class VersionedServer vx api m where
    versionedServer ::
        Proxy vx ->
        Proxy api ->
        m Void ->
        ServerT api m ->
        ServerT (Versioned vx :> api) m

instance (
    ServerT (UnVersioned api) m ~ ServerT api m,
    ServerVersion (UnVersioned api) (CompileVersion va api)) => VersionedServer '[va] api m where
    versionedServer _ api errF server = serverVersion uv cv errF server
        where
        uv = Proxy :: Proxy (UnVersioned api)
        cv = Proxy :: Proxy (CompileVersion va api)

instance (
    ServerT (UnVersioned api) m ~ ServerT api m,
    ServerVersion (UnVersioned api) (CompileVersion va api),
    VersionedServer '[vb] api m) => VersionedServer '[va, vb] api m where
    versionedServer _ api errF server =
        serverVersion uv cv errF server :<|> versionedServer vtail api errF server
        where
        uv = Proxy :: Proxy (UnVersioned api)
        cv = Proxy :: Proxy (CompileVersion va api)
        vtail = Proxy :: Proxy '[vb]

instance (
    ServerT (UnVersioned api) m ~ ServerT api m,
    ServerVersion (UnVersioned api) (CompileVersion va api),
    VersionedServer '[vb, vc] api m) => VersionedServer '[va, vb, vc] api m where
    versionedServer _ api errF server =
        serverVersion uv cv errF server :<|> versionedServer vtail api errF server
        where
        uv = Proxy :: Proxy (UnVersioned api)
        cv = Proxy :: Proxy (CompileVersion va api)
        vtail = Proxy :: Proxy '[vb, vc]

type family UnVersioned api where
    UnVersioned (api1 :<|> api2) = UnVersioned api1 :<|> UnVersioned api2
    UnVersioned (Versions vx :> api) = UnVersioned api
    UnVersioned (a :> api) = a :> UnVersioned api
    UnVersioned a = a
