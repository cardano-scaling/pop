module Pop.CliSpec where

import Pop.Cli (pop)
import Test.Hspec (Spec, it, shouldReturn)

spec :: Spec
spec =
  it "can display --help" $ do
    let args = ["--help"]

    pop args `shouldReturn` "PoP Command-line Tool - v0.1.0.0"
