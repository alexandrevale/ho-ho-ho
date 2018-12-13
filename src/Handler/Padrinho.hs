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
import Data.Maybe(fromJust)

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
    
formPadrinhoUpdate :: Text -> Text -> Form Padrinho
formPadrinhoUpdate telefone cpf = renderBootstrap $ Padrinho
    <$> areq textField "Telefone: "(Just telefone)
    <*> areq textField "CPF: "(Just cpf)
    
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
            
getPadrinhoUpdateR :: PadrinhoId -> Handler Html
getPadrinhoUpdateR padrinhoId = do
    logado <- lookupSession "_USR"
    (Padrinho telefone cpf) <- runDB $ get404 padrinhoId
    ((res, widgetForm ), enctype) <- runFormPost $ formPadrinhoUpdate telefone cpf
    case res of
        FormMissing -> defaultLayout $(whamletFile "templates/alterarpadrinho.hamlet")
        FormFailure x -> redirect $ PadrinhoUpdateR padrinhoId
        FormSuccess padrinho -> do 
            runDB $ do
                replace padrinhoId padrinho
            redirect HomeR
            
postPadrinhoUpdateR :: PadrinhoId -> Handler Html
postPadrinhoUpdateR padrinhoId = getPadrinhoUpdateR padrinhoId

getListarCriancaAdotadaR :: Handler Html
getListarCriancaAdotadaR = do
    logado <- lookupSession "_USR"
    let perfil = usuarioPerfil . read . unpack . fromJust $ logado
    sacolinha <- runDB $ selectList [SacolinhaAdotador ==. perfil] []
    crianca <- runDB $ selectList [CriancaId <-. (fmap (sacolinhaCriancaid . entityVal) sacolinha)] []
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/tabelas.lucius")
        $(whamletFile "templates/listar-crianca-adotada.hamlet")