{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

-- import Text.Lucius
-- import Text.Julius
import Import

formLogin :: Form (Text,Text)
formLogin = renderBootstrap $ (,) 
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getLoginR :: Handler Html
getLoginR = do 
    -- setTitle "Login - Ho Ho Ho"
    (widgetForm, enctype) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/login.hamlet")
    
postLoginR :: Handler Html 
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of
        FormSuccess ("admin@admin.com","admin1234") -> do 
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