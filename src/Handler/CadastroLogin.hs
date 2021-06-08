{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.CadastroLogin where

import Import
import Text.Lucius
import Handler.Form

formLoga :: Form (Loga, Text)
formLoga = renderDivs $ (,)
    <$>(Loga
        <$> areq textField "Email: " Nothing
        <*> areq passwordField "Senha: " Nothing
    )
    <*> areq passwordField "Confirme a senha: " Nothing
--pega as informações digitadas no formulario
getLogaR :: Handler Html
getLogaR = do
    (widget, _) <- generateFormPost formLoga
    msg <- getMessage 
    defaultLayout $ do
        (formWidget widget msg LogaR "Cadastrar")
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/form/form.lucius")
        
    

--Manda as informações digitadas na pagina de cadastro pro banco            
postLogaR :: Handler Html 
postLogaR = do
    ((result, _), _) <- runFormPost formLoga
    case result of
        FormSuccess (loga@(Loga email senha), conf) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Just _ -> do
                    setMessage [shamlet|
                        <div>
                            Email ja cadastrado, tente novamente com outro email
                    |]
                    redirect LogaR
                Nothing -> do
                    if senha == conf then do
                        runDB $ insert loga
                        setMessage [shamlet|

                        |]
                        redirect CadastroR
                    else do
                        setMessage [shamlet|
                            <h1>
                                SENHA E CONFIRMAÇÃO NAO CONFEREM
                        |]
                        redirect LogaR
        _ -> redirect HomeR

postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/form/form.lucius")
        $(whamletFile "templates/admin/admin.hamlet")

