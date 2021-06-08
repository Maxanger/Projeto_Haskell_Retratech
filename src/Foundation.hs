{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")
type Form a = Html -> MForm Handler (FormResult a, Widget)
instance Yesod App where
    makeLogger = return . appLogger
--Autorizações de paginas
    authRoute _ = Just AutentiR
    isAuthorized AutentiR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized CadastroR _ = return Authorized
    isAuthorized LogaR _ = return Authorized
    isAuthorized ContatoR _ = return Authorized
    isAuthorized SobreR _ = return Authorized
    isAuthorized ServicoR _ = isAdmin
    isAuthorized AdminR _ = isAdmin
    isAuthorized ServSolicAdminR _ = isAdmin
    isAuthorized ListaServAdminR _ = isAdmin
    isAuthorized _ _ = isUsuario

isAdmin :: Handler AuthResult
isAdmin = do
    sess <- lookupSession "_ID"
    case sess of
        Nothing -> return AuthenticationRequired
        Just "admin" -> return Authorized
        Just _ -> return (Unauthorized "LOGUE COMO ADMIN PARA PODER ACESSAR A PAGINAS DE CRIAÇÃO, EDIÇÃO E EXCLUSÃO\nDE SESSÃO DE FOTOS  \
                                        \USUARIO:admin@admin.com\nSENHA:admin")

isUsuario :: Handler AuthResult
isUsuario = do
    sess <- lookupSession "_ID"
    case sess of
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
