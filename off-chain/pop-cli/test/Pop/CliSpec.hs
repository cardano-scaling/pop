module Pop.CliSpec where

import Pop.Cli (pop)
import Test.Hspec (Spec, it, shouldReturn)

spec :: Spec
spec = do
  it "can display --help" $ do
    let args = ["--help"]

    pop args `shouldReturn` "PoP Command-line Tool - v0.1.0.0"

  it "can request antithesis run" $ do
    let args = ["run"
               , "--repository", "https://github.com/cardano-foundation/antithesis"
               , "--commit", "9114528e2343e6fcf3c92de71364275227e6b16d"
               ]

    pop args `shouldReturn` "PoP Command-line Tool - v0.1.0.0"
