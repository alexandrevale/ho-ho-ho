{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Perfil where

import Import
import Text.Lucius
import Text.Julius
import Prelude (read)

widgetNav :: Maybe Text -> Widget
widgetNav logado = $(whamletFile "templates/homenav.hamlet")

getPerfilR :: Handler Html   
getPerfilR = do
    logado <- lookupSession "_USR"
    defaultLayout $ do 
        -- toWidgetHead [hamlet|
        --     <script src=@{StaticR js_script_js}>
        -- |]
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/perfil.hamlet")
        toWidget $(luciusFile "templates/home.lucius")
        -- toWidgetHead $(juliusFile "templates/home.julius")