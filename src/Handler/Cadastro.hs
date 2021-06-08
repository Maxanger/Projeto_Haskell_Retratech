{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cadastro where

import Import
import Text.Lucius
import Handler.Form

formCadastro :: Maybe Usuario -> Form Usuario
formCadastro ms = renderDivs $ Usuario
    <$> areq textField "Nome: " (fmap usuarioNome ms)
    <*> areq textField "Sobrenome: " (fmap usuarioSobrenome ms)
    <*> areq textField "Digite Seu Endereço de Email novamente: " (fmap usuarioEmail ms)
    <*> areq textField "CPF : " (fmap usuarioCpf ms)
    <*> areq textField "Telefone: " (fmap usuarioTelefone ms)
    <*> areq textField "Endereço: " (fmap usuarioEndereco ms)

--pega as informações digitadas no formulario
getCadastroR :: Handler Html
getCadastroR = do
    (widget, _) <- generateFormPost (formCadastro Nothing)
    msg <- getMessage 
    defaultLayout $ do
        (formWidget widget msg CadastroR "Cadastrar")
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/form/form.lucius")

--Manda as informações digitadas na pagina de cadastro pro banco             
postCadastroR :: Handler Html 
postCadastroR = do
    ((result, _), _) <- runFormPost (formCadastro Nothing)
    case result of
        FormSuccess usuario -> do
            runDB $ insert usuario
            setMessage [shamlet|
                
            |]
            redirect HomeR
        _ -> redirect HomeR

--Mostra Informações do Perfil  
getPerfilR :: UsuarioId -> Handler Html
getPerfilR uid = do
    usuario <- runDB $ get404 uid
    defaultLayout [whamlet|
    <div class="container">
        <h1>
            INFORMAÇÕES DA CONTA
        <h2>
            Nome: #{usuarioNome usuario}
        <h2>
            Sobrenome: #{usuarioSobrenome usuario}
        <h2>
            CPF: #{usuarioCpf usuario}
        <h2>
            Telefone: #{usuarioTelefone usuario}
        <h2>
            Endereco: #{usuarioEndereco usuario}
    |]
