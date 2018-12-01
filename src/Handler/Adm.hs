{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

getAdminR :: Handler Html   
getAdminR = do 
    usuarios <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/admin.hamlet")

postApagarR :: UsuarioId -> Handler Html
postApagarR usrid = do 
    runDB $ delete usrid
    redirect AdminR
