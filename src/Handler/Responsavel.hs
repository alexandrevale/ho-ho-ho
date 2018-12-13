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

formResponsavel :: Form Responsavel
formResponsavel = renderBootstrap $ Responsavel
    <$> areq textField "Telefone: " Nothing
    <*> areq textField "CPF: " Nothing
    
getResponsavelR :: UsuarioId -> Handler Html 
getResponsavelR usuarioId = do 
    -- setTitle "Cadastro de Empresa - Ho Ho Ho"
    logado <- lookupSession "_USR"
    (widgetForm, enctype) <- generateFormPost formResponsavel
    defaultLayout $ do 
        setTitle "Cadastro de Responsável - Ho Ho Ho"
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/home.lucius")
        toWidget $(luciusFile "templates/cadastro-empresa.lucius")
        $(whamletFile "templates/cadastro-responsavel.hamlet")

postResponsavelR :: UsuarioId -> Handler Html
postResponsavelR usuarioId = do 
    ((res,_),_) <- runFormPost formResponsavel
    case res of 
        FormSuccess responsavel -> do 
            rid <- runDB $ do
                rid <- insert responsavel
                update usuarioId [UsuarioPerfil =. ResponsavelPerfil (fromSqlKey rid) ]
                return rid
            redirect $ CriancaR rid
        _ -> do
            setMessage [shamlet| <p> Deu ruim |]
            redirect $ ResponsavelR usuarioId