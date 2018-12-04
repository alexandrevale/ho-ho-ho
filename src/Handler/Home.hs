{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius
import Prelude (read)

getHomeR :: Handler Html
getHomeR = do 
    msg <- getMessage
    logado <- lookupSession "_USR"
    defaultLayout $ do 
        -- toWidgetHead [hamlet|
        --     <script src=@{StaticR js_script_js}>
        -- |]
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/home.hamlet")
        toWidget $(luciusFile "templates/home.lucius")
        -- toWidgetHead $(juliusFile "templates/home.julius")

getCadastroR :: Handler Html   
getCadastroR = do 
    defaultLayout $ do 
        -- toWidgetHead [hamlet|
        --     <script src=@{StaticR js_script_js}>
        -- |]
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/tipocadastro.hamlet")
        toWidget $(luciusFile "templates/home.lucius")
        toWidgetHead $(juliusFile "templates/home.julius")