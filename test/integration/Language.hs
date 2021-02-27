{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative    ((<$>), (<*>))
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Lazy   (toStrict)
import           Data.Foldable          (for_)
import qualified Data.HashMap.Strict    as HM (HashMap, empty,
                                               traverseWithKey)
import           Data.List
import           Data.Maybe             (fromMaybe, mapMaybe)
import qualified Data.Text              as T
import           Data.Yaml              as Y (FromJSON, Value (..), decodeEither',
                                              parseJSON, (.!=), (.:), (.:?))
import           Network.Wreq
import           System.FilePath
import           Test.Hspec
import           Text.Mustache


langspecs :: [String]
langspecs =
  [ "https://codeload.github.com/andrewthad/spec/legacy.tar.gz/add_list_context_check"
  , "https://codeload.github.com/mustache/spec/tar.gz/v1.1.3"
  ]


data LangSpecFile = LangSpecFile
  { overview :: String
  , tests    :: [LangSpecTest]
  }


data LangSpecTest = LangSpecTest
  { name            :: String
  , specDescription :: String
  , specData        :: Y.Value
  , template        :: T.Text
  , expected        :: T.Text
  , testPartials    :: HM.HashMap String T.Text
  }


instance FromJSON LangSpecFile where
  parseJSON (Y.Object o) = LangSpecFile
    <$> o .: "overview"
    <*> o .: "tests"
  parseJSON _ = mzero


instance FromJSON LangSpecTest where
  parseJSON (Y.Object o) = LangSpecTest
    <$> o .: "name"
    <*> o .: "desc"
    <*> o .: "data"
    <*> o .: "template"
    <*> o .: "expected"
    <*> o .:? "partials" .!= HM.empty
  parseJSON _ = mzero


getOfficialSpecRelease :: String -> IO [(String, LangSpecFile)]
getOfficialSpecRelease releaseURL  = do
    res <- get releaseURL
    let archive = Tar.read $ GZip.decompress (res ^. responseBody)
    either (error . show) (return . fromEntries) $ entriesToList archive
  where
    entriesToList :: Tar.Entries e -> Either e [Tar.Entry]
    entriesToList Tar.Done = Right []
    entriesToList (Tar.Fail e) = Left e
    entriesToList (Tar.Next entry rest) = (entry:) <$> entriesToList rest

    fromEntries =
      map decodeSpec
      . filter (not . isOptionalSpec)
      . filter isYamlFile
      . mapMaybe fromNormalFile

    fromNormalFile e =
      case Tar.entryContent e of
        Tar.NormalFile f _ -> Just (Tar.entryPath e, f)
        _ -> Nothing

    isYamlFile (filename, _) = takeExtension filename `elem` [".yml", ".yaml"]

    isOptionalSpec (filename, _) = "~" `isPrefixOf` takeFileName filename

    decodeSpec (filename, f) =
      let spec = case decodeEither' $ toStrict f of
            Left e -> error $ "Error parsing spec file " ++ filename ++ ": " ++ show e
            Right spec -> spec
      in (filename, spec)


testOfficialLangSpec :: [(String, LangSpecFile)] -> Spec
testOfficialLangSpec testfiles =
  for_ testfiles $ \(filename, LangSpecFile { tests }) ->
    describe ("File: " ++ takeFileName filename) $
      for_ tests $ \(LangSpecTest { .. }) ->
        it ("Name: " ++ name ++ "  Description: " ++ specDescription) $
          let
            compiled = do
              partials' <- HM.traverseWithKey compileTemplate testPartials
              template' <- compileTemplate name template
              return $ template' { partials = partials' }
          in
            case compiled of
              Left m -> expectationFailure $ show m
              Right tmp ->
                substituteValue tmp (toMustache specData) `shouldBe` expected


main :: IO ()
main =
  void $ do
    specs <- mapM getOfficialSpecRelease langspecs
    hspec $ mapM_ testOfficialLangSpec specs
