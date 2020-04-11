{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Database.PostgreSQL.Migrator

connParser :: Parser String
connParser = strOption
             ( long "conn"
            <> short 'c'
            <> metavar "CONN"
            <> help "PostgreSQL connection string" )

dirParser :: Parser String
dirParser = strOption
            ( long "dir"
           <> short 'd'
           <> metavar "DIR"
           <> help "directory of migration files" )

nameParser :: Parser String
nameParser = strOption
             ( long "name"
            <> short 'n'
            <> metavar "NAME"
            <> help "name of migration" )

stepsParser :: Parser (Maybe Int)
stepsParser = optional
            $ option auto
              ( long "steps"
             <> short 's'
             <> metavar "STEPS"
             <> help "number of migration steps need to be processed" )

setupParser :: Parser SetupOptions
setupParser = SetupOptions <$> connParser

genParser :: Parser GenOptions
genParser = GenOptions <$> dirParser <*> nameParser

upParser :: Parser UpOptions
upParser = UpOptions <$> connParser <*> dirParser <*> stepsParser

downParser :: Parser DownOptions
downParser = DownOptions <$> connParser <*> dirParser <*> stepsParser

optionsParser :: Parser Migration
optionsParser =
  subparser
    ( command "setup" (info (Setup <$> setupParser) ( progDesc "Setup migration schemas and tables" ))
   <> command "generate" (info (Gen <$> genParser) ( progDesc "Generate migration file" ))
   <> command "up" (info (Up <$> upParser) ( progDesc "Run migrations" ))
   <> command "down" (info (Down <$> downParser) ( progDesc "Revert migrations" ))
    )

main :: IO ()
main = do
  migration <- execParser opts
  result <- exec migration
  putStrLn result
  pure ()
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Run migrations on PostgreSQL database"
     <> header "" )
