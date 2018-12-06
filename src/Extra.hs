{-# LANGUAGE TemplateHaskell #-}

module Extra where

import Database.Persist.TH

data Perfil = PadrinhoPerfil {padrinhoPerfil :: Int } |
              ResponsavelPerfil { responsavelPerfil :: Int } |
              EmpresaPerfil { empresaPerfil :: Int }
              deriving (Show, Read, Eq)
derivePersistField "Perfil"