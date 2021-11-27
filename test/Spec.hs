import Test.Hspec
import qualified AlmanacSpec

-- NOTE(luis) not using hspec-discover because
-- it seems to trip up nix-shell
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Almanac" AlmanacSpec.spec
