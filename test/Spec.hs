import Test.Hspec

import HackageJson
import Expect.Hackage1


main :: IO ()
main = hspec $ do
  describe "hackage.json" $ do
    it "parses an experpt of hackage.json" $ do
      parseHackageJson "test/hackage1.json" `shouldReturn` hackageJson1
