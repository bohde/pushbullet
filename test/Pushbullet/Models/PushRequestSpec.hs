{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pushbullet.Models.PushRequestSpec where

import           Control.Applicative           ((<$>), (<*>))
import           Data.Aeson                    (decode, encode)
import           Pushbullet.Models.PushRequest
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances

instance Arbitrary Target where
  arbitrary = oneof [
      return All
    , Device <$> arbitrary
    , Email <$> arbitrary
    , Channel <$> arbitrary
    , Client <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary Message where
  arbitrary = oneof [
      Note <$> arbitrary <*> arbitrary
    , Link <$> arbitrary <*> arbitrary <*> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary PushRequest where
  arbitrary = PushRequest <$> arbitrary <*> arbitrary
  shrink = genericShrink

spec :: Spec
spec = do
  describe "PushRequest" $ do
    it "decode . encode should roundtrip" $ property $ \(p :: PushRequest) -> do
      (decode . encode) p `shouldBe` Just p
