{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

-- Definición de la tabla 'User' usando la sintaxis QuasiQuotes de Persistent
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    age  Int
    deriving Show
|]

main :: IO ()
main = runStdoutLoggingT $ withSqliteConn "test.db" $ \conn -> liftIO $ do
    runSqlConn (runMigration migrateAll) conn

    -- Insertar un usuario en la base de datos
    userId <- runSqlConn (insert $ User "Alicia" 25) conn

    -- Consultar todos los usuarios
    users <- runSqlConn (selectList [] [Asc UserName]) conn

    -- Mostrar los resultados
    putStrLn "Usuarios en la base de datos:"
    mapM_ (print . entityVal) users
