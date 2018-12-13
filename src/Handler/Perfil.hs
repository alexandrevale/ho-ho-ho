{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Perfil where

import Import
import Database.Persist.Sql(toSqlKey)
import Text.Lucius
import Text.Julius
import Prelude (read)
import Handler.Empresa(getEmpresaUpdateR)
import Handler.Padrinho(getPadrinhoUpdateR)

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

getPerfilR :: Handler Html   
getPerfilR = do
    logado <- lookupSession "_USR"
    case fmap (read . unpack) logado of
        Just (Usuario _ _ _ (PadrinhoPerfil x)) -> 
            getPadrinhoUpdateR (toSqlKey x)
            
        Just (Usuario _ _ _ (EmpresaPerfil x)) ->
            getEmpresaUpdateR (toSqlKey x)
        
        Just (Usuario _ _ _ (ResponsavelPerfil x)) ->
            undefined