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
    logado <- lookupSession "_USR"
    (widgetForm, enctype) <- generateFormPost formEmpresa
    defaultLayout $ do 
        setTitle "Cadastro de Empresa - Ho Ho Ho"
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
    logado <- lookupSession "_USR"
    (Empresa telefone endereco cnpj) <- runDB $ get404 empresaId
    ((res, widgetForm ), enctype) <- runFormPost $ formEmpresaUpdate telefone endereco cnpj
    case res of
        FormMissing -> defaultLayout $(whamletFile "templates/alterarempresa.hamlet")
        FormFailure x -> redirect $ EmpresaUpdateR empresaId
        FormSuccess empresa -> do 
            runDB $ do
                replace empresaId empresa
            redirect ListarCriancaAdotadaR
            
postEmpresaUpdateR :: EmpresaId -> Handler Html
postEmpresaUpdateR empresaId = getEmpresaUpdateR empresaId