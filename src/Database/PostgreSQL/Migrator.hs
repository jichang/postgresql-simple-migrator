{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Migrator where

import           System.Directory
import qualified Data.ByteString.Char8              as BS8 (pack)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           Database.PostgreSQL.Simple.Types   (Query (..))
import           Database.PostgreSQL.Simple.Time (ZonedTimestamp)
import           System.IO
import           Data.Time
import           Data.Time.Format
import           Data.Maybe
import           Data.List                          (sort, find, take)
import qualified Crypto.Hash.MD5                    as MD5 (hash)
import qualified Data.ByteString                    as BS (ByteString, readFile)
import qualified Data.ByteString.Base64             as B64 (encode)

data SetupOptions = SetupOptions
  { setupConnStr :: String
  } deriving (Show)

data GenOptions = GenOptions
  { genMigrationDir :: FilePath 
  , genMigrationName :: String
  } deriving (Show)

data UpOptions = UpOptions
  { upConnStr :: String
  , upMigrationDir :: FilePath
  , upMigrationSteps :: Maybe Int
  }
  deriving (Show)

data DownOptions = DownOptions
  { downConnStr :: String
  , downMigrationDir :: FilePath
  , downMigrationSteps :: Maybe Int
  } deriving (Show)

data Migration
  = Setup SetupOptions
  | Gen GenOptions
  | Up UpOptions
  | Down DownOptions
  deriving (Show)

exec :: Migration -> IO String
exec migration =
  case migration of
    Setup options ->
      setup options
    Gen options ->
      generate options
    Up options ->
      up options
    Down options ->
      down options

setup :: SetupOptions -> IO String
setup options = do
  conn <- connectPostgreSQL (BS8.pack (setupConnStr options))
  withTransaction conn (initializeSchema conn)

generate :: GenOptions -> IO String
generate options = do
  createDirectoryIfMissing True (genMigrationDir options)
  currTime <- getCurrentTime
  let dir = migrationDir ++ "/" ++ (targetDir currTime) ++ "_" ++ migrationName
  createDirectoryIfMissing True dir
  createFile (dir ++ "/" ++ "up.sql") "-- This file will be executed when running migrate up"
  createFile (dir ++ "/" ++ "down.sql") "-- This file will be executed when running migrate down"
  pure ""
  where
    migrationDir = genMigrationDir options
    migrationName = genMigrationName options
    targetDir :: UTCTime -> String
    targetDir =
      formatTime defaultTimeLocale "%Y%m%d%H%M%S"
    createFile filePath content =
      withFile filePath WriteMode
        (\handle -> do hPutStrLn handle content)

up :: UpOptions -> IO String
up options = do
  let migrationDir = upMigrationDir options
  conn <- connectPostgreSQL (BS8.pack (upConnStr options))
  migrationNames <- fmap sort (listDirectory migrationDir)
  migrations <- readMigrationFiles UpSql migrationDir migrationNames
  records <- getMigrationRecords conn
  runMigrations conn records migrations
  pure ""

down :: DownOptions -> IO String
down options = do
  let migrationDir = downMigrationDir options
  conn <- connectPostgreSQL (BS8.pack (downConnStr options))
  records <- getMigrationRecords conn
  revertMigrations conn (take steps records) migrationDir
  pure ""
  where
    steps =
      case downMigrationSteps options of
        Just n -> n
        Nothing -> 1

initializeSchema :: Connection -> IO String
initializeSchema conn = do
  execute_ conn createSchema
  execute_ conn createTable
  pure ""
  where
    createSchema =
      "CREATE SCHEMA IF NOT EXISTS migration;"
    createTable =
      "CREATE TABLE IF NOT EXISTS migration.records( \n\
      \  migration VARCHAR NOT NULL, \n\
      \  checksum VARCHAR NOT NULL, \n\
      \  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now() \n\
      \)"

type CheckSum = BS.ByteString;

data MigrationRecord = MigrationRecord
  { recordMigration :: String
  , recordCheckSum :: CheckSum
  , recordCreatedAt :: ZonedTimestamp
  } deriving (Show, Read)

instance FromRow MigrationRecord where
    fromRow = MigrationRecord <$>
        field <*> field <*> field

instance ToRow MigrationRecord where
    toRow (MigrationRecord name checksum executedAt) =
       [toField name, toField checksum, toField executedAt]

getMigrationRecords :: Connection -> IO [MigrationRecord]
getMigrationRecords conn = do
  query_ conn "SELECT migration, checksum, created_at FROM migration.records ORDER BY migration ASC;"

runMigrations :: Connection -> [MigrationRecord] -> [(String, BS.ByteString, CheckSum)] -> IO ()
runMigrations conn records migrations = do
  sequence $ fmap process $ filter isValid migrations
  pure ()
  where
    isSameMigration (migrationName, migrationScript, checksum) record =
      (recordMigration record == migrationName) && (recordCheckSum record == checksum)

    isValid migration =
      isNothing $ find (isSameMigration migration) records

    process migration = do
      withTransaction conn (run migration)

    run :: (String, BS.ByteString, BS.ByteString) -> IO ()
    run (migrationName, migrationScript, checksum) = do
      execute_ conn (Query migrationScript)
      execute conn "INSERT INTO migration.records(migration, checksum) VALUES(?, ?)" (migrationName, checksum)
      pure ()

revertMigrations :: Connection -> [MigrationRecord] -> String -> IO ()
revertMigrations conn records migrationDir = do
  migrations <- readMigrationFiles DownSql migrationDir migrationNames
  sequence $ fmap process migrations
  pure ()
  where
    migrationNames = fmap recordMigration records

    process migration = do
      withTransaction conn (revert migration)

    revert :: (String, BS.ByteString, BS.ByteString) -> IO ()
    revert (migrationName, migrationScript, checksum) = do
      execute_ conn (Query migrationScript)
      execute conn "DELETE FROM migration.records WHERE migration = ?" ([migrationName])
      pure ()

data MigrationFileType
  = UpSql
  | DownSql

readMigrationFiles :: MigrationFileType -> String -> [String] -> IO [(String, BS.ByteString, BS.ByteString)]
readMigrationFiles migrationFileType migrationDir migrationNames =
  sequence $ fmap read migrationNames
  where
    fileName =
      case migrationFileType of
        UpSql -> "up.sql"
        DownSql -> "down.sql"
    read :: String -> IO (String, BS.ByteString, BS.ByteString)
    read migrationName = do
      migrationScript <- BS.readFile (migrationDir ++ "/" ++ migrationName ++ "/" ++ fileName)
      pure (migrationName, migrationScript, B64.encode $ MD5.hash migrationScript)
