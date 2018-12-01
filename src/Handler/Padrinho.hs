{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Padrinho where

import Import

formPadrinho :: Form Padrinho
formPadrinho = renderBootstrap $ Padrinho
    <$> areq textField "Nome: " Nothing
    <*> areq textField "E-mail: " Nothing
    <*> areq textField "Senha: " Nothing
    <*> areq textField "Confirmação de Senha: " Nothing
    <*> areq textField "Telefone: " Nothing
    <*> areq textField "CPF: " Nothing
    
getPadrinhoR :: Handler Html
getPadrinhoR = do 
    (widgetForm, enctype) <- generateFormPost formPadrinho
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/padrinho.hamlet")

postPadrinhoR :: Handler Html
postPadrinhoR = do 
    ((res,_),_) <- runFormPost formPadrinho
    case res of 
        FormSuccess ator -> do 
            runDB $ insert padrinho 
            redirect PadrinhoR
        _ -> redirect HomeR

getPadrinhosR :: Handler Html
getPadrinhosR = do 
    -- select * from ator;
    padrinhos <- runDB $ selectList [] [Asc PadrinhoNome]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/padrinhos.hamlet")

getPerfilR :: PadrinhoId -> Handler Html
getPerfilR atorid = do 
    padrinho <- runDB $ get404 padrinhoid
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/perfil.hamlet")