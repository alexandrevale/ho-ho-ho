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

formEmpresaUpdate :: Text -> Text -> Text -> Form Empresa
formEmpresaUpdate telefone endereco cnpj = renderBootstrap $ Empresa
    <$> areq textField "Telefone: " (Just telefone)
    <*> areq textField "Endereço: " (Just endereco)
    <*> areq textField "CNPJ: " (Just cnpj)
    
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
            
getEmpresaUpdateR :: EmpresaId -> Handler Html
getEmpresaUpdateR empresaId = do
    (Empresa telefone endereco cnpj) <- runDB $ get404 empresaId
    -- pega o formulario e retorna widgetForm que permite usar o hamlet junto com o enctype poe no formulario e o res é indica se eh um post ou get
    -- x é um array de erros
    ((res, widgetForm ), enctype) <- runFormPost $ formEmpresaUpdate telefone endereco cnpj
    case res of
        FormMissing -> defaultLayout $(whamletFile "templates/alterarempresa.hamlet")
        FormFailure x -> redirect $ EmpresaUpdateR empresaId
        FormSuccess empresa -> do 
            runDB $ do
                replace empresaId empresa
            redirect HomeR
            
postEmpresaUpdateR :: EmpresaId -> Handler Html
postEmpresaUpdateR empresaId = getEmpresaUpdateR empresaId