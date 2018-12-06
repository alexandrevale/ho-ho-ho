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

formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $  pure (,)
    <*> (Usuario 
            <$> areq textField "Nome: " Nothing
            <*> areq emailField "E-mail: " Nothing 
            <*> areq passwordField "Senha: " Nothing
    )
    <*> areq passwordField "Confirme sua senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do 
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
                runDB $ insert usr 
                redirect CadastroR
            else do 
                setMessage [shamlet|
                    <div .alert .alert-danger>
                        <strong>Senhas não conferem
                |]
                redirect UsuarioR
        _ -> redirect UsuarioR
        

