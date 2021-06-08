{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Form where

import Import
import Text.Lucius
--Formulario para simplificar 
formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg rota m = $(whamletFile "templates/form/form.hamlet")
    