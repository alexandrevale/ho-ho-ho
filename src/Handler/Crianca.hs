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


formCrianca :: Form Crianca
formCrianca = renderBootstrap $ Crianca
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Idade: " Nothing
    <*> areq textField "Sexo: " Nothing
    <*> areq textField "RG: " Nothing
    <*> areq textField "Tamanho da roupa: " Nothing
    <*> areq textField "Tamanho do calçado: " Nothing
    <*> areq textField  "Preferencia: " Nothing

getCriancaR :: ResponsavelId -> Handler Html
getCriancaR responsavelId = do 
    (widgetForm, enctype) <- generateFormPost formCrianca
    logado <- lookupSession "_USR"
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/usuario.lucius")
        toWidget $(luciusFile "templates/listar-crianca.lucius")
        $(whamletFile "templates/cadastro-crianca.hamlet")

postCriancaR :: ResponsavelId -> Handler Html
postCriancaR responsavelId = do 
    ((res,_),_) <- runFormPost formCrianca
    case res of 
        FormSuccess crianca -> do 
            runDB $ insert crianca
          --  update responsavelId [CriancaPerfil =. PadrinhoPerfil (fromSqlKey pid) ] 
            redirect HomeR

getListarCriancaR :: Handler Html
getListarCriancaR = do
    crianca <- runDB $ selectList [] [Asc CriancaNome]
    logado <- lookupSession "_USR"
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/listar-crianca.hamlet")

