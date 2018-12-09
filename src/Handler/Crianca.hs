{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Crianca where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formCrianca :: Form Crianca
formCrianca = renderBootstrap $ Crianca
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Idade: " Nothing
    <*> areq textField "Sexo: " Nothing
    <*> areq textField "RG: " Nothing
    <*> areq textField "Tamanho da roupa: " Nothing
    <*> areq textField "Tamanho do calçado: " Nothing
    <*> areq textField  "Preferencia: " Nothing

getCriancaR :: Handler Html
getCriancaR = do 
    (widgetForm, enctype) <- generateFormPost formCrianca
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/usuario.lucius")
        toWidget $(luciusFile "templates/cadastro-empresa.lucius")
        $(whamletFile "templates/cadastro-crianca.hamlet")

postCriancaR :: Handler Html
postCriancaR = do 
    ((res,_),_) <- runFormPost formCrianca
    case res of 
        FormSuccess crianca -> do 
            runDB $ insert crianca 
            redirect HomeR
