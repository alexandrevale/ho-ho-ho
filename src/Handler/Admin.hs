{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Text.Lucius
-- import Database.Persist.Sql
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

getAdminR :: Handler Html   
getAdminR = do 
    logado <- lookupSession "_USR"
    usuarios <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/admin.hamlet")

{--postApagarR :: UsuarioId -> Handler Html
postApagarR usrid = do 
    runDB $ delete usrid
    redirect AdminR--}

getListarUsuarioR :: Handler Html
getListarUsuarioR = do
    logado <- lookupSession "_USR"
    usuario <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/listar-usuario.hamlet")
        
getListarMensagensR :: Handler Html
getListarMensagensR = do
    logado <- lookupSession "_USR"
    mensagens <- runDB $ selectList [] [Asc ContatoNome]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/listar-mensagens.hamlet")
        



