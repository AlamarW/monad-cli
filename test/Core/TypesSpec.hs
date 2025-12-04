module Core.TypesSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import Core.Types

-- | Test suite for Core.Types module
spec :: Spec
spec = do
  describe "Value" $ do
    it "can represent text values" $ do
      let val = VText (pack "hello")
      val `shouldBe` VText (pack "hello")

    it "can represent integer values" $ do
      let val = VInt 42
      val `shouldBe` VInt 42

    it "can represent file paths" $ do
      let val = VPath "/tmp/file.txt"
      val `shouldBe` VPath "/tmp/file.txt"

    it "can represent boolean values" $ do
      let val = VBool True
      val `shouldBe` VBool True

  describe "Record" $ do
    it "can store multiple values" $ do
      let record = Map.fromList
            [ (pack "path", VPath "/tmp/file.txt")
            , (pack "size", VInt 1024)
            , (pack "name", VText (pack "file.txt"))
            ]
      Map.size record `shouldBe` 3

    it "can retrieve values by key" $ do
      let record = Map.fromList [(pack "name", VText (pack "test"))]
      Map.lookup (pack "name") record `shouldBe` Just (VText (pack "test"))

  describe "ErrorInfo" $ do
    it "captures error context" $ do
      let err = ErrorInfo
            { errorCommand = pack "test-command"
            , errorMessage = pack "test error"
            , errorInput = Nothing
            }
      errorCommand err `shouldBe` pack "test-command"
      errorMessage err `shouldBe` pack "test error"
      errorInput err `shouldBe` Nothing
