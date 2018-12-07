{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Responsavel where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formResponsavel :: Form Responsavel
formResponsavel = renderBootstrap $ Responsavel
    <$> areq textField "Telefone: " Nothing
    <*> areq textField "CPF: " Nothing
    
getResponsavelR :: UsuarioId -> Handler Html 
getResponsavelR usuarioId = do 
    -- setTitle "Cadastro de Empresa - Ho Ho Ho"
    (widgetForm, enctype) <- generateFormPost formResponsavel
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/home.lucius")
        toWidget $(luciusFile "templates/cadastro-empresa.lucius")
        $(whamletFile "templates/cadastro-responsavel.hamlet")

postResponsavelR :: UsuarioId -> Handler Html
postResponsavelR usuarioId = do 
    ((res,_),_) <- runFormPost formResponsavel
    case res of 
        FormSuccess responsavel -> do 
            runDB $ do
                rid <- insert responsavel
                update usuarioId [UsuarioPerfil =. ResponsavelPerfil (fromSqlKey rid) ]
            redirect HomeR