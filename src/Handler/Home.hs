{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where
import           Yesod
import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
-- import Text.Julius
-- import Settings.StaticFiles
-- import Prelude (read)

getHomeR :: Handler Html
getHomeR = do 
    -- msg <- getMessage
    logado <- lookupSession "_USR"
    defaultLayout $ do 
        setTitle "Home - Ho Ho Ho"
        addScriptRemote "https://code.jquery.com/jquery-3.3.1.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/OwlCarousel2/2.3.4/owl.carousel.js"
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/OwlCarousel2/2.3.4/assets/owl.carousel.css"
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/OwlCarousel2/2.3.4/assets/owl.theme.default.css"
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/home.hamlet")
        toWidget $(luciusFile "templates/home.lucius")
        -- toWidgetBody $(juliusFile "templates/home.julius")
        addScript $ StaticR js_script_js
        
        


getCadastroR :: Handler Html   
getCadastroR = do 
    defaultLayout $ do 
        -- toWidgetHead [hamlet|
        --     <script src=@{StaticR js_script_js}>
        -- |]
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/tipocadastro.hamlet")
        toWidget $(luciusFile "templates/tipocadastro.lucius")
        toWidget $(luciusFile "templates/home.lucius")
        -- toWidgetHead $(juliusFile "templates/home.julius")

getPerfilR :: Handler Html   
getPerfilR = do 
    defaultLayout $ do 
        -- toWidgetHead [hamlet|
        --     <script src=@{StaticR js_script_js}>
        -- |]
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/perfil.hamlet")
        toWidget $(luciusFile "templates/home.lucius")
        -- toWidgetHead $(juliusFile "templates/home.julius")