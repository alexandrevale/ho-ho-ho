{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Yesod
import Import
import Text.Lucius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Prelude (read)

-- widgetNav :: Maybe Text -> Widget
-- widgetNav logado = $(whamletFile "templates/homenav.hamlet")

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

formLogin :: Form (Text,Text)
formLogin = renderBootstrap $ (,) 
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getLoginR :: Handler Html
getLoginR = do 
    -- setTitle "Login - Ho Ho Ho"
    logado <- lookupSession "_USR"
    (widgetForm, enctype) <- generateFormPost formLogin
    defaultLayout $ do 
        setTitle "Login - Ho Ho Ho"
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/login.hamlet")
        toWidget $(luciusFile "templates/login.lucius")
    
postLoginR :: Handler Html 
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of
        FormSuccess ("admin@admin.com","admin1234") -> do 
            setSession "_USR" (pack $ show $ Usuario "admin" "admin@admin.com" "" (PadrinhoPerfil 0) )
            redirect AdminR
        
        FormSuccess (email,senha) -> do
            logado <- runDB $ selectFirst [UsuarioEmail ==. email,
                                          UsuarioSenha ==. senha] []
            case logado of
                Just (Entity usrid usuario) -> do 
                    setSession "_USR" (pack $ show usuario)
                    setMessage [shamlet|
                    |]
                    redirect ListarCriancaAdotadaR
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