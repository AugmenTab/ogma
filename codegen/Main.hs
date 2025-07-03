{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Header (hUserAgent)

import Generate.Language (generateLanguageModules)
import Subtags.Subtag (parseSubtags)

main :: IO ()
main = do
  req <- HTTP.parseRequest subtagRegistry
  response <-
    HTTP.httpLBS $
      HTTP.setRequestHeader
        hUserAgent
        ["ogma/0.1 (https://github.com/AugmenTab/ogma)"]
        req

  let
    (_allFields, _usedConstructors, _valsPerTag, subtags) =
      parseSubtags
        . TE.decodeUtf8
        . LBS.toStrict
        $ HTTP.getResponseBody response

  case NEL.nonEmpty $ Map.elems subtags of
    Just neSubtags -> generateLanguageModules neSubtags
    Nothing -> putStrLn "Whoopsie"

  -- putStrLn $ "All Fields: " <> show allFields
  -- putStrLn $ "Values per Tag: " <> show valsPerTag
  -- putStrLn $ "Subtags: " <> show subtags

  -- TIO.writeFile "test.txt" . T.unlines $ concatMap subtagNames subtags
  -- TIO.putStrLn . T.unlines $ subtagConstructor <$> Map.elems subtags
  -- mapM_ (putStrLn . T.unpack . subtagConstructor) subtags

subtagRegistry :: String
subtagRegistry =
  "https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry"
