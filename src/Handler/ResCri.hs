{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.ResCri where

import Import

formResponsavel :: Form Responsavel
formResponsavel = renderBootstrap $ Responsavel
    <$> areq textField "Nome: " Nothing
    <*> areq emailField  "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing
    <*> areq passwordField "Confirmação de Senha: " Nothing
    <*> areq textField "Telefone: " Nothing
    <*> areq textField "CPF: " Nothing
    
formCrianca :: Form Crianca
formCrianca = renderBootstrap $ Crianca
    <$> areq textField "Nome: " Nothing
    <*> areq intField "Idade: " Nothing
    <*> areq textField "Sexo: " Nothing
    <*> areq textField "Tamanho da roupa: " Nothing
    <*> areq intField "Tamanho do calçado: " Nothing
    <*> areq textareaField  "Preferencia: " Nothing
    
getResponsavelR :: Handler Html
getResponsavelR = do 
    (widgetForm, enctype) <- generateFormPost formResponsavel
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/responsavel.hamlet")

getCriancaR :: Handler Html
getCriancaR = do 
    (widgetForm, enctype) <- generateFormPost formCrianca
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/crianca.hamlet")

postResponsavelR :: Handler Html
postResponsavelR = do 
    ((res,_),_) <- runFormPost formResponsavel
    case res of 
        FormSuccess responsavel -> do 
            runDB $ insert responsavel 
            redirect ResponsavelR
        _ -> redirect HomeR
        
postCriancaR :: Handler Html
postCriancaR = do 
    ((res,_),_) <- runFormPost formCrianca
    case res of 
        FormSuccess crianca -> do 
            runDB $ insert crianca 
            redirect CriancaR
        _ -> redirect HomeR

getResponsavelR :: Handler Html
getResponsavelR = do 
    -- select * from responsavel;
    responsavel <- runDB $ selectList [] [Asc ResponsavelNome]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/responsavel.hamlet")

getCriancaR :: Handler Html
getCriancaR = do 
    -- select * from responsavel;
    crianca <- runDB $ selectList [] [Asc ResponsavelCrianca]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/crianca.hamlet")

getPerfilR :: ResponsavelId -> Handler Html
getPerfilR responsavelid = do 
    responsavel <- runDB $ get404 responsavelid
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/perfil.hamlet")

getPerfilR :: CriancaId -> Handler Html
getPerfilR criancaid = do 
    crianca <- runDB $ get404 responsavelid
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/perfil.hamlet")
