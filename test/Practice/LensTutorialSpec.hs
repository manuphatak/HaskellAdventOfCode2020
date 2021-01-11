module Practice.LensTutorialSpec (spec) where

import Control.Lens
import Practice.LensTutorial
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "lenses" $ do
    it "can view a value" $ do
      view _1 ("goal", "match") `shouldBe` "goal"
    it "can modify a value" $ do
      over _1 (++ "!!!") ("goal", "match") `shouldBe` ("goal!!!", "match")
    it "can set a value" $ do
      set _1 "SET" ("goal", "match") `shouldBe` ("SET", "match")

    let epicMeetup = Meetup "After after party" (34.3705, -119.1391)

    it "creates lens for custom types" $ do
      view name epicMeetup `shouldBe` "After after party"

    it "is composable" $ do
      let meetupLongitude = location . _2
      set meetupLongitude (-120) epicMeetup `shouldBe` epicMeetup {_location = (34.3705, -120)}

    it "can access members" $ do
      (epicMeetup ^. location . _1) `shouldBe` 34.3705

    it "can set members" $ do
      (location . _2 .~ 13 $ epicMeetup) `shouldBe` epicMeetup {_location = (34.3705, 13)}
    it "can set members" $ do
      (location . _2 %~ pred $ epicMeetup) `shouldBe` epicMeetup {_location = (34.3705, -120.1391)}

  describe "prisms" $ do
    it "" $ do
      preview _Left (Left "Hello") `shouldBe` Just "Hello"
    it "" $ do
      preview _Left (Right "Hello" :: Either String String) `shouldBe` Nothing

    it "" $ do
      review _Left "Howdy" `shouldBe` (Left "Howdy" :: Either String String)

    it "" $ do
      Left "Hi" ^? _Left `shouldBe` Just "Hi"

  describe "traversals" $ do
    it "can include any or all elements" $ do
      toListOf traverse [1 :: Int .. 5] `shouldBe` [1 .. 5]

      [1 :: Int .. 5] ^.. traverse `shouldBe` [1 .. 5]

    it "can get the first item" $ do
      firstOf traverse [1 :: Int .. 5] `shouldBe` Just 1

    it "can get the last item" $ do
      lastOf traverse [1 :: Int .. 5] `shouldBe` Just 5
