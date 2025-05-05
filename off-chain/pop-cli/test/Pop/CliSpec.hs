module Pop.CliSpec where

import Pop.Cli (Result (..), pop)
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec (Spec, it, shouldReturn, shouldThrow)

spec :: Spec
spec = do
  it "can display --help" $ do
    let args = ["--help"]

    pop args `shouldThrow` \e -> e == ExitSuccess

  it "can request antithesis run" $ do
    let args =
          [ "request",
            "--platform",
            "github",
            "--repository",
            "cardano-foundation/antithesis",
            "--commit",
            "9114528e2343e6fcf3c92de71364275227e6b16d"
          ]

    pop args `shouldReturn` RequestOK {txId = "7db484475883c0b5a36a4b0d419b45fae0b64d770bc0b668d063d21d59489ad8"}
