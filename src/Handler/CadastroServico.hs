{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.CadastroServico where

import Import
import Text.Lucius
import Handler.Form

formServico :: Maybe Servico -> Form Servico
formServico ms = renderDivs $ Servico
    <$> areq textField "Nome da Sessao: " (fmap servicoNome ms)
    <*> areq intField "Quantidade de Fotos: " (fmap servicoQtd_fotos ms)
    <*> areq doubleField "Valor: " (fmap servicoValor ms)

--pega as informações digitadas no formulario
getServicoR :: Handler Html
getServicoR = do
    (widget, _) <- generateFormPost (formServico Nothing)
    msg <- getMessage 
    defaultLayout $ do
        (formWidget widget msg ServicoR "Cadastrar")
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/form/form.lucius")
        
--Manda as informações digitadas na pagina de cadastro pro banco          
postServicoR :: Handler Html
postServicoR = do
    ((result, _), _) <- runFormPost (formServico Nothing)
    case result of
        FormSuccess servico -> do
            runDB $ insert servico
            setMessage [shamlet|
                
            |]
            redirect ServicoR
        _ -> redirect HomeR


postApagarServR :: ServicoId -> Handler Html
postApagarServR sid = do
    runDB $ delete sid
    redirect ListaServAdminR

getEditarServR :: ServicoId -> Handler Html
getEditarServR sid = do
    servico <- runDB $ get404 sid
    (widget, _) <- generateFormPost (formServico (Just servico))
    msg <- getMessage 
    defaultLayout $ do
        (formWidget widget msg (EditarServR sid) "Editar")
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/form/form.lucius")

postEditarServR :: ServicoId -> Handler Html
postEditarServR sid = do
    _ <- runDB $ get404 sid
    ((result, _), _) <- runFormPost (formServico Nothing)
    case result of
        FormSuccess novoServico -> do
            runDB $ replace sid novoServico
            redirect ListaServAdminR
        _ -> redirect HomeR

getListaServR :: Handler Html
getListaServR = do
    servicos <- runDB $ selectList [] [Asc ServicoNome]
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/listaserv/listaserv.lucius")
        $(whamletFile "templates/listaserv/listaserv.hamlet")

getListaServAdminR :: Handler Html
getListaServAdminR = do
    servicos <- runDB $ selectList [] [Asc ServicoNome]
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/listaserv_admin/listaserv_admin.lucius")
        $(whamletFile "templates/listaserv_admin/listaserv_admin.hamlet")