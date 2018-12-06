{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Empresa where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formEmpresa :: Form Empresa
formEmpresa = renderBootstrap $ Empresa
    <$> areq textField "Telefone: " Nothing
    <*> areq textField "Endereço: " Nothing
    <*> areq textField "CNPJ: " Nothing
    
getEmpresaR :: UsuarioId -> Handler Html 
getEmpresaR usuarioId = do 
    -- setTitle "Cadastro de Empresa - Ho Ho Ho"
    (widgetForm, enctype) <- generateFormPost formEmpresa
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/home.lucius")
        toWidget $(luciusFile "templates/cadastro-empresa.lucius")
        $(whamletFile "templates/cadastro-empresa.hamlet")

postEmpresaR :: UsuarioId -> Handler Html
postEmpresaR usuarioId = do 
    ((res,_),_) <- runFormPost formEmpresa
    case res of 
        FormSuccess empresa -> do 
            runDB $ do
                eid <- insert empresa
                update usuarioId [UsuarioPerfil =. EmpresaPerfil (fromSqlKey eid) ]
            redirect HomeR
            
