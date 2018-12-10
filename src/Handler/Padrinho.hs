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



formPadrinho :: Form Padrinho
formPadrinho = renderBootstrap $ Padrinho
    <$> areq textField "Telefone: " Nothing
    <*> areq textField "CPF: " Nothing
    
getPadrinhoR :: UsuarioId -> Handler Html 
getPadrinhoR usuarioId = do 
    logado <- lookupSession "_USR"
    (widgetForm, enctype) <- generateFormPost formPadrinho
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/home.lucius")
        toWidget $(luciusFile "templates/cadastro-padrinho.lucius")
        $(whamletFile "templates/cadastro-padrinho.hamlet")

postPadrinhoR :: UsuarioId -> Handler Html
postPadrinhoR usuarioId = do 
    ((res,_),_) <- runFormPost formPadrinho
    case res of 
        FormSuccess padrinho -> do 
            runDB $ do
                pid <- insert padrinho
                update usuarioId [UsuarioPerfil =. PadrinhoPerfil (fromSqlKey pid) ]
            redirect HomeR