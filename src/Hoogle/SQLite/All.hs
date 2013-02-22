{-# LANGUAGE OverloadedStrings #-}

module Hoogle.SQLite.All
    (SQLiteDataBase
    ,module Hoogle.SQLite.All
    ) where

import Hoogle.SQLite.Type
import Hoogle.Type.All
import Hoogle.Score.All
import Hoogle.Store.All
import Hoogle.DataBase.Items
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString
import Data.ByteString.Unsafe
import Data.Serialize
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified Data.Text as T
import Database.SQLite3 as SQLite
import System.Directory (doesFileExist)

createSQLiteDataBase :: [SQLiteDataBase] -> String -> Input -> IO SQLiteDataBase
createSQLiteDataBase deps dbpath (facts,xs) = do
    exists <- doesFileExist dbpath
    bracket (SQLite.open (T.pack dbpath)) SQLite.close $ \db -> do
        unless exists (createSQLiteSchema db)
        createSQLiteItems db xs
        -- createSQLiteAliases db deps facts
        -- createSQLiteInstances db deps facts
        -- createSQLiteTypeSearch db as is tys
        -- createSQLiteSuggest db deps facts
        return ()
    return $ SQLiteDataBase 0

createSQLiteSchema :: SQLite.Database -> IO ()
createSQLiteSchema db = do
    SQLite.exec db $ T.unlines
        [ "BEGIN;"

        , "CREATE TABLE"
        , "    Entry"
        , "    ("
        , "        entryId        INTEGER NOT NULL,"
        , "        entryPackageId INTEGER NOT NULL,"
        , "        entryModuleId  INTEGER NOT NULL,"
        , "        entryName      TEXT NOT NULL,"
        , "        entryFullName  TEXT NOT NULL,"
        , "        entryType      INTEGER,"
        , "        entryText      NONE NOT NULL,"
        , "        entryDocs      NONE NOT NULL,"
        , "        entryUrl       TEXT,"
        , "        entryPriority  INTEGER NOT NULL"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    Location"
        , "    ("
        , "        locationId  INTEGER PRIMARY KEY AUTOINCREMENT,"
        , "        locationUrl TEXT NOT NULL"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    EntryLoc"
        , "    ("
        , "        entryLocId     INTEGER PRIMARY KEY AUTOINCREMENT,"
        , "        entryId        INTEGER NOT NULL,"
        , "        locationId     INTEGER NOT NULL,"
        , "        entryContextId INTEGER NOT NULL"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    Package"
        , "    ("
        , "        packageId   INTEGER PRIMARY KEY AUTOINCREMENT,"
        , "        packageName TEXT NOT NULL"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    Module"
        , "    ("
        , "        moduleId   INTEGER PRIMARY KEY AUTOINCREMENT,"
        , "        moduleName TEXT NOT NULL"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    TypeGraph"
        , "    ("
        , "        graphData NONE NOT NULL -- a blob of binary data"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    Suggest"
        , "    ("
        , "        suggestName  TEXT NOT NULL,"
        , "        suggestCtor  TEXT,"
        , "        suggestWhich INTEGER NOT NULL, -- 0 for Data, 1 for Class"
        , "        suggestText  TEXT NOT NULL,"
        , "        suggestKind  INTEGER NOT NULL"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    Alias"
        , "    ("
        , "        aliasId     INTEGER PRIMARY KEY AUTOINCREMENT,"
        , "        aliasNameId INTEGER NOT NULL,"
        , "        aliasArgs   TEXT NOT NULL,"
        , "        aliasType   NONE NOT NULL"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    Name"
        , "    ("
        , "        nameId   INTEGER PRIMARY KEY AUTOINCREMENT,"
        , "        nameText TEXT NOT NULL"
        , "    );"
        , ""
        , "CREATE TABLE"
        , "    Instance"
        , "    ("
        , "        instanceNameId       INTEGER NOT NULL,"
        , "        instanceTargetNameId INTEGER NOT NULL"
        , "    );"

        , "COMMIT;"
        ]

data SQLiteStatements = SQLiteStatements
    { entryInsertStmt :: SQLite.Statement
    , urlInsertStmt   :: SQLite.Statement
    }

createSQLiteStatements :: SQLite.Database -> IO SQLiteStatements
createSQLiteStatements db =
    SQLiteStatements
    <$> (SQLite.prepare db $ T.unwords
         [ "INSERT INTO Entry"
         , "    ( entryId"
         , "    , entryPackageId"
         , "    , entryModuleId"
         , "    , entryName"
         , "    , entryFullName"
         , "    , entryType"
         , "    , entryText"
         , "    , entryDocs"
         , "    , entryUrl"
         , "    , entryPriority"
         , "    )"
         , "VALUES"
         , "    ( ?1, ?2, ?3, ?4, 5, ?6, ?7, ?8, ?9, ?10 );"
         ])
    <*> (SQLite.prepare db
         "INSERT OR REPLACE INTO Location (LocationUrl) VALUES (?1);")

finalizeSQLiteStatements :: SQLiteStatements -> IO ()
finalizeSQLiteStatements stmts = do
    SQLite.finalize (entryInsertStmt stmts)
    SQLite.finalize (urlInsertStmt stmts)

createSQLiteItems :: SQLite.Database -> [TextItem] -> IO ()
createSQLiteItems db xs = do
    SQLite.exec db "BEGIN;"
    bracket
        (createSQLiteStatements db)
        finalizeSQLiteStatements
        (\stmts -> do
              forM_ (entriesItems (createItems xs))
                    (createSQLiteItem (entryInsertStmt stmts)))
    SQLite.exec db "COMMIT;"

bindAndRun :: SQLite.Statement -> [SQLite.SQLData] -> IO ()
bindAndRun stmt xs = do
    SQLite.bind stmt xs
    SQLite.step stmt
    SQLite.reset stmt
    SQLite.clearBindings stmt

withObject :: Storable a => a -> (ByteString -> IO ()) -> IO ()
withObject x f = allocaBytes len $ \ptr -> do
    poke ptr x
    unsafePackCStringFinalizer (castPtr ptr) len (return ()) >>= f
  where
    len = sizeOf x

createSQLiteItem :: SQLite.Statement -> Once Entry -> IO ()
createSQLiteItem insertStmt x = do
    let x' = fromOnce x
    bindAndRun insertStmt
        [ SQLite.SQLInteger (fromIntegral (onceKey x))
        , SQLite.SQLInteger 1
        , SQLite.SQLInteger 1
        , SQLite.SQLText (T.pack (entryKey x'))
        , SQLite.SQLText (T.pack (entryName x'))
        , SQLite.SQLNull
        , SQLite.SQLBlob (encode (entryText x'))
        , SQLite.SQLBlob (encode (entryDocs x'))
        , SQLite.SQLNull
        , SQLite.SQLInteger (fromIntegral (entryPriority x'))
        ]
