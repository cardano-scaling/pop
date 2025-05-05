module Pop.CliSpec where

import Pop.Cli (Result (..), pop)
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec (Spec, it, shouldReturn, shouldThrow)

spec :: Spec
spec = do
    it "can display --help" $ do
        let args = ["--help"]

        pop args `shouldThrow` \e -> e == ExitSuccess

    it "can request user registration" $ do
        let args =
                [ "register"
                , "--platform"
                , "github"
                , "--username"
                , "bob"
                , "--pubkeyhash"
                , "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                ]

        pop args `shouldReturn` RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

    it "can request adding user to a project" $ do
        let args =
                [ "add-user"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--role"
                , "maintainer"
                , "--user-id"
                , "github/bob"
                ]

        pop args `shouldReturn` RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

    it "can request antithesis run" $ do
        let args =
                [ "request"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--commit"
                , "9114528e2343e6fcf3c92de71364275227e6b16d"
                ]

        pop args `shouldReturn` RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}

    it "can request removing user from a project" $ do
        let args =
                [ "remove-user"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--user-id"
                , "github/bob"
                ]

        pop args `shouldReturn` RequestOK{txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}
