{-# LANGUAGE TemplateHaskell #-}

module Extra where

import Database.Persist.TH
import Data.Int
data Perfil = PadrinhoPerfil {padrinhoPerfil :: Int64 } |
              ResponsavelPerfil { responsavelPerfil :: Int64 } |
              EmpresaPerfil { empresaPerfil :: Int64 }
              deriving (Show, Read, Eq)
derivePersistField "Perfil"