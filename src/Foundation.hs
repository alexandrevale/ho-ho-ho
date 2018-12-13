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
    isAuthorized LogoutR _ = ehUsuario
    isAuthorized AdminR _ = ehAdmin
    isAuthorized CadastroR _ = return Authorized
    isAuthorized TelaR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized (DeletarUsuarioR _) _ = ehAdmin
    isAuthorized ListarMensagensR _ = ehAdmin
    isAuthorized ListarUsuarioR _ = ehAdmin
    isAuthorized ListarCriancaR _ = ehUsuario
    isAuthorized (EmpresaR _) _ = return Authorized
    isAuthorized (ResponsavelR _) _ = return Authorized
    isAuthorized (PadrinhoR _) _ = return Authorized
    isAuthorized (CriancaR _) _ = return Authorized
    isAuthorized PerfilR _ = ehUsuario
    isAuthorized (EmpresaUpdateR _) _ = ehUsuario
    isAuthorized (PadrinhoUpdateR _) _ = ehUsuario
    isAuthorized (SacolinhaR _) _ = ehUsuario
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized ListarCriancaAdotadaR _ = ehUsuario
    
    
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

ehUsuario :: Handler AuthResult
ehUsuario = do 
    logado <- lookupSession "_USR"
    case logado of
        Just _ -> return Authorized
        Nothing -> return AuthenticationRequired
        
ehAdmin :: Handler AuthResult
ehAdmin = do 
    logado <- lookupSession "_USR"
    case logado of
        Just usuario -> do 
            nome <- return $ usuarioNome $ read $ unpack usuario
            if nome == "admin" then
                return Authorized
            else 
                return $ Unauthorized "Não tem acesso!"
        Nothing -> return AuthenticationRequired