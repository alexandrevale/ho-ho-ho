{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Crianca where

import Import

formCrianca :: Form Crianca
formCrianca = renderBootstrap $ Crianca
    <$> areq textField "Nome: " Nothing
    <*> areq textField "E-mail: " Nothing
    <*> areq textField "Senha: " Nothing
    <*> areq textField "Confirmação de Senha: " Nothing
    <*> areq textField "Telefone: " Nothing
    <*> areq textField "CPF: " Nothing
    
getCriancaR :: Handler Html
getCriancaR = do 
    (widgetForm, enctype) <- generateFormPost formCrianca
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/crianca.hamlet")

postCriancaR :: Handler Html
postCriancaR = do 
    ((res,_),_) <- runFormPost formCrianca
    case res of 
        FormSuccess crianca -> do 
            runDB $ insert crianca 
            redirect CriancaR
        _ -> redirect HomeR

getCriancaR :: Handler Html
getCriancaR = do 
    -- select * from responsavel;
    crianca <- runDB $ selectList [] [Asc ResponsavelCrianca]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/crianca.hamlet")

getPerfilR :: CriancaId -> Handler Html
getPerfilR criancaid = do 
    crianca <- runDB $ get404 responsavelid
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/perfil.hamlet")