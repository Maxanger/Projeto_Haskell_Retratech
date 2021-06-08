{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Lucius

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        --para adicionar o bootstrap
        --verifica qual usuario está logado, pode alterar o tipo de menu   
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/home/home.lucius")
        $(whamletFile "templates/home/home.hamlet")