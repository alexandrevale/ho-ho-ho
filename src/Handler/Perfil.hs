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
widgetNav logado = do
                    addStylesheet $ StaticR css_bootstrap_css
                    $(whamletFile "templates/homenav.hamlet") 
                    toWidget $(luciusFile "templates/homenav.lucius")

widgetFooter :: Widget
widgetFooter = do
                addStylesheet $ StaticR css_bootstrap_css
                $(whamletFile "templates/footer.hamlet") 
                toWidget $(luciusFile "templates/footer.lucius")


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