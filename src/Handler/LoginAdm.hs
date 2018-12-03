{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.LoginAdm where

import Text.Lucius
import Text.Julius
import Import

formLoginadm :: Form (Text,Text)
formLoginadm = renderBootstrap $ (,) 
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getLoginadmR :: Handler Html
getLoginadmR = do 
    (widgetForm, enctype) <- generateFormPost formLoginadm
    msg <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/login.hamlet")
    
postLoginadmR :: Handler Html 
postLoginadmR = do 
    ((res,_),_) <- runFormPost formLoginadm
    case res of
        FormSuccess ("admin@admin.com","admin123") -> do 
            setSession "_USR" (pack $ show $ Usuario "admin" "admin@admin.com" "")
            redirect AdminR
        
        FormSuccess (email,senha) -> do
            logado <- runDB $ selectFirst [UsuarioEmail ==. email,
                                          UsuarioSenha ==. senha] []
            case logado of
                Just (Entity usrid usuario) -> do 
                    setSession "_USR" (pack $ show usuario)
                    setMessage [shamlet|
                        <h1>
                            Usuario logado
                    |]
                    redirect HomeR
                Nothing -> do 
                    setMessage [shamlet|
                        <h1>
                            Usuario e senha n encontrados!
                    |]
                    redirect LoginR
        _ -> redirect LoginR
        
postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_USR"
    redirect HomeR