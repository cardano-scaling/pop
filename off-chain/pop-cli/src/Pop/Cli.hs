{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Pop.Cli where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Options.Applicative
  ( Parser,
    command,
    defaultPrefs,
    execParserPure,
    fullDesc,
    handleParseResult,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strOption,
    subparser,
    value,
    (<**>),
  )

type Args = [String]

type Platform = String

type SHA1 = String

type TxId = String

data Command
  = Request {platform :: Platform, repository :: String, commit :: SHA1, directory :: String}
  | Register {platform :: Platform, username :: String, pubkeyhash :: String}
  | AddUser {platform :: Platform, repository :: String, role :: String, userId :: String}

requestOptions :: Parser Command
requestOptions =
  Request
    <$> strOption
      ( long "platform"
          <> short 'p'
          <> metavar "PLATFORM"
          <> help "The platform to run the test on"
      )
    <*> strOption
      ( long "repository"
          <> short 'r'
          <> metavar "REPOSITORY"
          <> help "The repository URL or path"
      )
    <*> strOption
      ( long "commit"
          <> short 'c'
          <> metavar "COMMIT"
          <> help "The commit hash or reference"
      )
    <*> strOption
      ( long "directory"
          <> short 'd'
          <> metavar "DIRECTORY"
          <> value "."
          <> help "Directory to run in (defaults to \".\")"
      )

registerOptions :: Parser Command
registerOptions =
  Register
    <$> strOption
      ( long "platform"
          <> short 'p'
          <> metavar "PLATFORM"
          <> help "The platform to register on"
      )
    <*> strOption
      ( long "username"
          <> short 'u'
          <> metavar "USERNAME"
          <> help "The username to register"
      )
    <*> strOption
      ( long "pubkeyhash"
          <> metavar "PUBKEYHASH"
          <> help "The public key hash for the user"
      )

addUserOptions :: Parser Command
addUserOptions =
  AddUser
    <$> strOption
      ( long "platform"
          <> short 'p'
          <> metavar "PLATFORM"
          <> help "The platform where the repository is hosted"
      )
    <*> strOption
      ( long "repository"
          <> short 'r'
          <> metavar "REPOSITORY"
          <> help "The repository URL or path"
      )
    <*> strOption
      ( long "role"
          <> metavar "ROLE"
          <> help "The role to assign to the user (e.g., maintainer, contributor)"
      )
    <*> strOption
      ( long "user-id"
          <> metavar "USER-ID"
          <> help "The ID of the user to add"
      )

commandParser :: Parser Command
commandParser =
  subparser
    ( command "request" (info requestOptions (progDesc "Request a test on a specific platform"))
      <> command "register" (info registerOptions (progDesc "Register a new user"))
      <> command "add-user" (info addUserOptions (progDesc "Add a user to a repository"))
    )

parseArgs :: [String] -> IO Command
parseArgs args = handleParseResult $ execParserPure defaultPrefs opts args
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Antithesis CLI"
            <> header "pop - A tool for managing Antithesis test runs using PoP"
        )

data Result
  = RequestOK {txId :: TxId}
  | RegisterOK {userId :: String}
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

pop :: Args -> IO Result
pop args =
  parseArgs args >>= \case
    Request {platform, repository, commit, directory} -> runTest platform repository commit directory
    Register {platform, username, pubkeyhash} -> registerUser platform username pubkeyhash
    AddUser {platform, repository, role, userId} -> addUserToRepo platform repository role userId

runTest :: Platform -> String -> SHA1 -> String -> IO Result
runTest _platform _repository _commit _directory = pure $ RequestOK {txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

registerUser :: Platform -> String -> String -> IO Result
registerUser _platform _username _pubkeyhash = pure $ RequestOK {txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

addUserToRepo :: Platform -> String -> String -> String -> IO Result
addUserToRepo _platform _repository _role _userId = pure $ RequestOK {txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}
