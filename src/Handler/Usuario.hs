{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql
import Database.Persist.Postgresql
import Network.HTTP.Types.Status
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

formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $  pure (,)
    <*> (Usuario 
            <$> areq textField "Nome: " Nothing
            <*> areq emailField "E-mail: " Nothing 
            <*> areq passwordField "Senha: " Nothing
            <*> areq ( selectField (optionsPairs ([( "Padrinho" , PadrinhoPerfil 0 ) , 
                                                  ( "Responsavel", ResponsavelPerfil 0) , 
                                                  ( "Empresa", EmpresaPerfil 0)] :: [( Text, Perfil )] ) ) ) 
                                                  "Perfil: " Nothing 
            
        )
            <*> areq passwordField "Confirme sua senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do 
    logado <- lookupSession "_USR"
    -- setTitle "Cadastro Base - Ho Ho Ho"
    (widgetUsu, enctype) <- generateFormPost formUsuario
    msg <- getMessage --mensagem que avisa se o usuarioc cadastrou certo
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/usuario.hamlet")
        toWidget $(luciusFile "templates/usuario.lucius")

postUsuarioR :: Handler Html
postUsuarioR = do 
    ((res,_),_) <- runFormPost formUsuario
    case res of
        FormSuccess (usr, passwordC) -> do 
            if (usuarioSenha usr) == passwordC then do
                uid <- runDB $ insert usr
                redirect $ case usuarioPerfil usr of
                    PadrinhoPerfil      _ -> PadrinhoR uid
                    ResponsavelPerfil   _ -> ResponsavelR uid
                    EmpresaPerfil       _ -> EmpresaR uid
            else do 
                setMessage [shamlet|
                    <div .alert .alert-danger>
                        <strong>Senhas não conferem
                |]
                redirect UsuarioR
        _ -> redirect UsuarioR