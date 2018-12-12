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


formCrianca :: ResponsavelId -> Form Crianca
formCrianca idresponsavel = renderBootstrap $ Crianca
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Idade: " Nothing
    <*> areq textField "Sexo: " Nothing
    <*> areq textField "RG: " Nothing
    <*> areq textField "Tamanho da roupa: " Nothing
    <*> areq textField "Tamanho do calçado: " Nothing
    <*> areq textField  "Preferencia: " Nothing
    <*> pure idresponsavel 

getCriancaR :: ResponsavelId -> Handler Html
getCriancaR responsavelId = do 
    (widgetForm, enctype) <- generateFormPost (formCrianca responsavelId)
    logado <- lookupSession "_USR"
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/usuario.lucius")
        toWidget $(luciusFile "templates/cadastro-empresa.lucius")
        $(whamletFile "templates/cadastro-crianca.hamlet")

postCriancaR :: ResponsavelId -> Handler Html
postCriancaR responsavelId = do 
    ((res,_),_) <- runFormPost (formCrianca responsavelId)
    case res of 
        FormSuccess crianca -> do 
            runDB $ insert crianca
            redirect HomeR

getListarCriancaR :: Handler Html
getListarCriancaR = do
    crianca <- runDB $ selectList [] [Asc CriancaNome]
    logado <- lookupSession "_USR"
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/listar-crianca.hamlet")
        
postSacolinhaR :: CriancaId -> Handler Html
postSacolinhaR  criancaId = do 
    logado <- lookupSession "_USR"
    case fmap (read . unpack) logado of
        Just (Usuario _ _ _ x) -> do
            _ <- runDB $ insert $ Sacolinha criancaId x
            redirect HomeR

