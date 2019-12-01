import           Combo
import           Test.Hspec
import           Test.QuickCheck hiding (choose)

naturals :: Gen Integer
naturals = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

main :: IO ()
main = hspec $ do
  describe "n choose k" $ do
    it "returns 0 when k > n" $ do
      1 `choose` 2  `shouldBe` 0
    it "returns n when k = 1" $ do
      10 `choose` 1 `shouldBe` 10
    it "returns 1 when n = k" $ do
      10 `choose` 10 `shouldBe` 1
    it "identity1" $ property $ do
      forAll naturals (\n -> div (n * (n - 1)) 2 == (n `choose` 2 :: Integer))
    it "identity2" $ property $ do
      forAll naturals (\n k -> n `choose` k == n `choose` (n - k))
