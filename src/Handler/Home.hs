{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where
import Yesod
import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Prelude (read)
import Database.Persist.Sql

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

getHomeR :: Handler Html
getHomeR = do
    logado <- lookupSession "_USR"
    (widgetCtt, enctype) <- generateFormPost formContato
    defaultLayout $ do 
        setTitle "Home - Ho Ho Ho"
        addScriptRemote "https://code.jquery.com/jquery-3.3.1.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/OwlCarousel2/2.3.4/owl.carousel.js"
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/OwlCarousel2/2.3.4/assets/owl.carousel.css"
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/OwlCarousel2/2.3.4/assets/owl.theme.default.css"
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/home.hamlet")
        toWidget $(luciusFile "templates/home.lucius")
        -- toWidgetBody $(juliusFile "templates/home.julius")
        addScript $ StaticR js_script_js
        
getCadastroR :: Handler Html   
getCadastroR = do 
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/home.lucius")
        
getTelaR :: Handler Html 
getTelaR  = do 
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/tipocadastro.lucius")
        $(whamletFile "templates/tipocadastro.hamlet")


formContato :: Form Contato
formContato = renderBootstrap $ Contato
    <$> areq textField "Nome: " Nothing
    <*> areq textField "E-mail: " Nothing
    <*> aopt textareaField "Mensagem: " Nothing
    
postContatoR :: Handler Html
postContatoR = do 
    ((res,_),_) <- runFormPost formContato
    case res of 
        FormSuccess contato -> do 
            runDB $ do
                insert contato
            redirect HomeR