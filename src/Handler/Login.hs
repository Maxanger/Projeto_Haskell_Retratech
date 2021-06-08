{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Text.Lucius
import Handler.Form

formLogin :: Form Loga
formLogin = renderDivs $ Loga
        <$> areq textField "Email: " Nothing
        <*> areq passwordField "Senha: " Nothing

--pega as informações digitadas no formulario
getAutentiR :: Handler Html
getAutentiR = do
    (widget, _) <- generateFormPost formLogin
    msg <- getMessage 
    defaultLayout $ do
        (formWidget widget msg AutentiR "Entrar")
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/form/form.lucius")

--Manda as informações digitadas na pagina de cadastro pro banco
postAutentiR :: Handler Html 
postAutentiR = do
    ((result, _), _) <- runFormPost formLogin
    case result of
        --acesso ao admin
        FormSuccess (Loga "admin@admin.com" "admin") -> do
            setSession "_ID" "admin"
            redirect AdminR
        FormSuccess (Loga email senha) -> do
            emailExiste <- runDB $ getBy (UniqueEmail email)
            case emailExiste of
                Nothing -> do
                    setMessage [shamlet|
                            Usuario nao encontrado
                    |]
                    redirect AutentiR
                Just (Entity _ loga) -> do
                    if senha == logaSenha loga then do
                        --setando sessão para ele se lembrar que está logado  
                        setSession "_ID" (logaEmail loga)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            Usuario nao encontrado
                        |]
                        redirect AutentiR
        _ -> redirect HomeR