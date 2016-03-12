{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pushbullet.Models.PushSpec where

import           Control.Applicative    ((<$>), (<*>))
import           Data.Aeson             (eitherDecode, encode)
import           Pushbullet.Models.Push
import           Test.Hspec

spec :: Spec
spec = do
  describe "User" $ do
    it "should be able to decode reference data" $ do
      let
        reference = "{\"created\": 1.381092887398433e+09, \"email\": \"elon@teslamotors.com\", \"email_normalized\": \"elon@teslamotors.com\", \"iden\": \"ujpah72o0\", \"image_url\": \"https://static.pushbullet.com/missing-image/55a7dc-45\", \"max_upload_size\": 2.62144e+07, \"modified\": 1.441054560741007e+09, \"name\": \"Elon Musk\"}"
        expected :: Either String User
        expected = Right $ User {userIden = "ujpah72o0",userCreated = 1.3810929e9,userModified = 1.4410546e9,userEmail = "elon@teslamotors.com",userEmailNormalized = "elon@teslamotors.com",userName = "Elon Musk",userImageUrl = "https://static.pushbullet.com/missing-image/55a7dc-45",userMaxUploadSize = 26214400,userReferredCount = Nothing,userReferrerIden = Nothing}
      eitherDecode reference `shouldBe` expected
  describe "Push" $ do
    it "should be able to decode reference data" $ do
      let
        reference = "{\"active\": true,\"body\": \"Space Elevator, Mars Hyperloop, Space Model S (Model Space?)\",\"created\": 1.412047948579029e+09,\"direction\": \"self\",\"dismissed\": false,\"iden\": \"ujpah72o0sjAoRtnM0jc\",\"modified\": 1.412047948579031e+09,\"receiver_email\": \"elon@teslamotors.com\",\"receiver_email_normalized\": \"elon@teslamotors.com\",\"receiver_iden\": \"ujpah72o0\",\"sender_email\": \"elon@teslamotors.com\",\"sender_email_normalized\": \"elon@teslamotors.com\",\"sender_iden\": \"ujpah72o0\",\"sender_name\": \"Elon Musk\",\"title\": \"Space Travel Ideas\",\"type\": \"note\"}"
        expected :: Either String Push
        expected = Right $ Push {pushIden = "ujpah72o0sjAoRtnM0jc", pushActive = True, pushCreated = 1.412048e9, pushModified = 1.412048e9, pushType = Note, pushDismissed = False, pushGuid = Nothing, pushDirection = Self, pushSenderIden = "ujpah72o0", pushSenderEmail = "elon@teslamotors.com", pushSenderEmailNormalized = "elon@teslamotors.com", pushSenderName = "Elon Musk", pushReceiverIden = "ujpah72o0", pushReceiverEmail = "elon@teslamotors.com", pushReceiverEmailNormalized = "elon@teslamotors.com", pushTargetDeviceIden = Nothing, pushSourceDeviceIden = Nothing, pushClientIden = Nothing, pushChannelIden = Nothing, pushAwakeAppGuids = Nothing, pushTitle = Just "Space Travel Ideas", pushBody = Just "Space Elevator, Mars Hyperloop, Space Model S (Model Space?)", pushUrl = Nothing, pushFileName = Nothing, pushFileType = Nothing, pushFileUrl = Nothing, pushImageUrl = Nothing, pushImageWidth = Nothing, pushImageHeight = Nothing}
      eitherDecode reference `shouldBe` expected
  describe "PushRequest" $ do
    it "should be able to decode reference data" $ do
      let
        reference = "{\"body\":\"Space Elevator, Mars Hyperloop, Space Model S (Model Space?)\",\"title\":\"Space Travel Ideas\",\"type\":\"note\"}"
        expected :: Either String PushRequest
        expected = Right $ PushRequest {pushRequestType = Note, pushRequestTitle = Just "Space Travel Ideas", pushRequestBody = Just "Space Elevator, Mars Hyperloop, Space Model S (Model Space?)", pushRequestUrl = Nothing, pushRequestFileName = Nothing, pushRequestFileType = Nothing, pushRequestFileUrl = Nothing, pushRequestSourceDeviceIden = Nothing, pushRequestDeviceIden = Nothing, pushRequestClientIden = Nothing, pushRequestChannelTag = Nothing, pushRequestEmail = Nothing, pushRequestGuid = Nothing}
      eitherDecode reference `shouldBe` expected
