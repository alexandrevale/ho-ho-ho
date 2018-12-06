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

formEmpresa :: Form Empresa
formEmpresa = renderBootstrap $ Empresa
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Telefone: " Nothing
    <*> areq textField "Endereço: " Nothing
    <*> areq textField "CNPJ: " Nothing
    
getEmpresaR :: Handler Html
getEmpresaR = do 
    -- setTitle "Cadastro de Empresa - Ho Ho Ho"
    (widgetForm, enctype) <- generateFormPost formEmpresa
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/home.lucius")
        toWidget $(luciusFile "templates/cadastro-empresa.lucius")
        $(whamletFile "templates/cadastro-empresa.hamlet")

postEmpresaR :: Handler Html
postEmpresaR = do 
    ((res,_),_) <- runFormPost formEmpresa
    case res of 
        FormSuccess empresa -> do 
            runDB $ insert empresa 
            redirect HomeR