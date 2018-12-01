{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Responsavel where

import Import

formResponsavel :: Form Responsavel
formResponsavel = renderBootstrap $ Responsavel
    <$> areq textField "Nome: " Nothing
    <*> areq textField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing
    <*> areq textField "Confirmação de Senha: " Nothing
    <*> areq textField "Telefone: " Nothing
    <*> areq textField "CPF: " Nothing
    
getResponsavelR :: Handler Html
getResponsavelR = do 
    (widgetForm, enctype) <- generateFormPost formResponsavel
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/responsavel.hamlet")

postResponsavelR :: Handler Html
postResponsavelR = do 
    ((res,_),_) <- runFormPost formResponsavel
    case res of 
        FormSuccess responsavel -> do 
            runDB $ insert responsavel 
            redirect ResponsavelR
        _ -> redirect HomeR

getResponsavelR :: Handler Html
getResponsavelR = do 
    -- select * from responsavel;
    responsavel <- runDB $ selectList [] [Asc ResponsavelNome]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/responsavel.hamlet")

getPerfilR :: ResponsavelId -> Handler Html
getPerfilR responsavelid = do 
    responsavel <- runDB $ get404 responsavelid
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/perfil.hamlet")