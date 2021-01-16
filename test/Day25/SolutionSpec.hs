module Day25.SolutionSpec (spec) where

import Day25.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day25/input.txt"
    part1 input `shouldBe` "8329514"

  xit "solves Part 2" $ do
    input <- readFile "./test/Day25/input.txt"
    part2 input `shouldBe` "hello_santa"

  let cardsPrivateKey = 8
  let cardsPublicKey = 5764801
  let doorsPrivateKey = 11
  let doorsPublicKey = 17807724
  let encryptionKey = 14897079
  describe "transform" $ do
    context "given cardsPrivateKey" $
      it "is cardsPublicKey" $ do
        transform 7 cardsPrivateKey `shouldBe` cardsPublicKey
    context "given doorsPrivateKey" $
      it "is doorsPublicKey" $ do
        transform 7 doorsPrivateKey `shouldBe` doorsPublicKey
  describe "crack" $ do
    context "given cardsPublicKey" $
      it "is cardsPrivateKey" $ do
        crack 7 11 cardsPublicKey `shouldBe` cardsPrivateKey
    context "given doorsPublicKey" $
      it "is doorsPrivateKey" $ do
        crack 7 11 doorsPublicKey `shouldBe` doorsPrivateKey
  describe "findEncryptionKey" $ do
    it "is the encryptionKey" $ do
      findEncryptionKey 7 11 (doorsPublicKey, cardsPublicKey) `shouldBe` encryptionKey
    it "is the encryptionKey" $ do
      findEncryptionKey 7 11 (cardsPublicKey, doorsPublicKey) `shouldBe` encryptionKey
