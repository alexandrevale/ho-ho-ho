{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Padrinho where

import Import
import Text.Lucius
import Text.Julius

formPadrinho :: Form Padrinho
formPadrinho = renderBootstrap $ Padrinho
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Telefone: " Nothing
    <*> areq textField "CPF: " Nothing
    
getPadrinhoR :: Handler Html
getPadrinhoR = do 
    setTitle "Cadastro de Padrinho - Ho Ho Ho"
    (widgetForm, enctype) <- generateFormPost formPadrinho
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/home.lucius")
        toWidget $(luciusFile "templates/cadastro-padrinho.lucius")
        $(whamletFile "templates/cadastro-padrinho.hamlet")

postPadrinhoR :: Handler Html
postPadrinhoR = do 
    ((res,_),_) <- runFormPost formPadrinho
    case res of 
        FormSuccess padrinho -> do 
            runDB $ insert padrinho 
            redirect HomeR