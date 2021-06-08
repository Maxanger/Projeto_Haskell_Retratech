{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.SolicServ where

import Import
import Text.Lucius
import Handler.Form
import Database.Persist.Postgresql

formSolicServ :: UsuarioId -> Form SolicServ
formSolicServ uid = renderDivs $ SolicServ
    <$> pure uid
    <*> areq (selectField servCB) "Sessão: " Nothing
    <*> areq textField "Observação: " Nothing
    <*> areq textField "Endereco: " Nothing

servCB = do
    servico <- runDB $ selectList [] [Asc ServicoNome]
    optionsPairs $
        map (\r -> (servicoNome $ entityVal r, entityKey r)) servico


--pega as informações digitadas no formulario
getSolicR :: UsuarioId -> Handler Html
getSolicR uid = do
    (widget, _) <- generateFormPost (formSolicServ uid)
    msg <- getMessage 
    defaultLayout $ do
        (formWidget widget msg (SolicR uid) "Cadastrar")
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/form/form.lucius")

--Manda as informações digitadas na pagina de cadastro pro banco   
postSolicR :: UsuarioId -> Handler Html 
postSolicR uid = do
    ((result, _), _) <- runFormPost (formSolicServ uid)
    case result of
        FormSuccess solic -> do
            runDB $ insert solic
            setMessage [shamlet|
                
            |]
            redirect HomeR
        _ -> redirect HomeR

getMeuServR :: UsuarioId -> Handler Html
getMeuServR uid = do
    let sql = "SELECT ??, ??, ?? FROM servico \
            \ INNER JOIN solic_serv ON solic_serv.servid = servico.id \
            \ INNER JOIN usuario ON solic_serv.usuid = usuario.id \
            \ WHERE usuario.id = ?"
    usuario <- runDB $ get404 uid
    tudo <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Servico, Entity SolicServ, Entity Usuario)]
    defaultLayout $ do
        $(whamletFile "templates/meusserv/meusserv.hamlet")
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/meusserv/meusserv.lucius")


getServSolicAdminR :: Handler Html
getServSolicAdminR = do
    let sql = "SELECT ??, ??, ?? FROM servico \
            \ INNER JOIN solic_serv ON solic_serv.servid = servico.id \
            \ INNER JOIN usuario ON solic_serv.usuid = usuario.id"
    tudo <- runDB $ rawSql sql [] :: Handler [(Entity Servico, Entity SolicServ, Entity Usuario)]
    defaultLayout $ do
        $(whamletFile "templates/meusservadmin/meusservadmin.hamlet")
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/meusservadmin/meusservadmin.lucius")
