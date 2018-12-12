{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import Prelude (read)


data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized ContatoR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized LogoutR _ = return Authorized
    isAuthorized AdminR _ = return Authorized
    isAuthorized CadastroR _ = return Authorized
    isAuthorized TelaR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized (DeletarUsuarioR _) _ = return Authorized
    isAuthorized ListarMensagensR _ = return Authorized
    isAuthorized ListarUsuarioR _ = return Authorized
    isAuthorized ListarCriancaR _ = return Authorized
    isAuthorized (EmpresaR _) _ = return Authorized
    isAuthorized (ResponsavelR _) _ = return Authorized
    isAuthorized (PadrinhoR _) _ = return Authorized
    isAuthorized (CriancaR _) _ = return Authorized
    isAuthorized PerfilR _ = return Authorized
    isAuthorized (EmpresaUpdateR _) _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    
    
    
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

