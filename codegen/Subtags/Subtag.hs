{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Subtags.Subtag
  ( Subtag (..)
  , parseSubtags
  ) where

import Data.List (foldl')
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

import Subtags.Sanitize (toConstructor)

data Subtag =
  Subtag
    { subtagType :: SubtagType
    , subtagCode :: T.Text
    , subtagNames :: NEL.NonEmpty T.Text
    , subtagConstructor :: T.Text
    } deriving (Show)

type UsedConstructors = Set T.Text
type ValuesPerTag = Map T.Text (Set T.Text)
type SubtagsPerCode = Map T.Text Subtag

data SubtagType
  = SubtagLanguage
  deriving (Show)

parseSubtags :: T.Text
             -> (Set T.Text, UsedConstructors, ValuesPerTag, SubtagsPerCode)
parseSubtags =
  foldl' parseBlock (Set.empty, Set.empty, Map.empty, Map.empty)
    . T.splitOn "\n%%\n"

parseBlock :: (Set T.Text, UsedConstructors, ValuesPerTag, SubtagsPerCode)
           -> T.Text
           -> (Set T.Text, UsedConstructors, ValuesPerTag, SubtagsPerCode)
parseBlock (allFields, usedConstructors, valuesPerTag, subtags) block =
  let
    kv = foldr insertField Map.empty $ T.lines block
    insertField line acc =
      case T.splitOn ": " line of
        [k, v] | not (T.null v) ->
          Map.insertWith
            (<>)
            k
            [T.strip v]
            acc
        _ ->
          acc
  in
    case Map.lookup "Type" kv of
      Just ["language"]
        | Just ["special"] /= Map.lookup "Scope" kv
        , isNothing (Map.lookup "Deprecated" kv) ->
            let
              mbSubtag = do
                desc <- NEL.nonEmpty =<< Map.lookup "Description" kv
                subtag <- NEL.head <$> (NEL.nonEmpty =<< Map.lookup "Subtag" kv)

                let
                  rawConstructor = toConstructor $ NEL.head desc
                  constructor =
                    if Set.member rawConstructor usedConstructors
                      then rawConstructor <> "_" <> subtag
                      else rawConstructor

                pure $
                  Subtag
                    { subtagType = SubtagLanguage
                    , subtagCode = subtag
                    , subtagNames = desc
                    , subtagConstructor = constructor
                    }

              subtagValuesPerTag =
                Map.fromList $
                  catMaybes
                    [ Just ("Type", Set.singleton "language")
                 -- , ("Subtag",) . Set.fromList <$> Map.lookup "Subtag" kv
                 -- , ("Description",) . Set.fromList <$> Map.lookup "Description" kv
                 -- , ("Comments",) . Set.fromList <$> Map.lookup "Comments" kv
                 -- , ("Deprecated",) . Set.fromList <$> Map.lookup "Deprecated" kv
                    , ("Macrolanguage",) . Set.fromList <$> Map.lookup "Macrolanguage" kv
                 -- , ("Preferred-Value",) . Set.fromList <$> Map.lookup "Preferred-Value" kv
                    , ("Scope",) . Set.fromList <$> Map.lookup "Scope" kv
                    , ("Suppress-Script",) . Set.fromList <$> Map.lookup "Suppress-Script" kv
                    ]
            in
              ( Set.union allFields . Set.fromList $ Map.keys kv
              , maybe
                  usedConstructors
                  (flip Set.insert usedConstructors . subtagConstructor)
                  mbSubtag
              , Map.unionWith (<>) subtagValuesPerTag valuesPerTag
              , case (Map.lookup "Subtag" kv, mbSubtag) of
                  (Just [code], Just subtag) -> Map.insert code subtag subtags
                  _ -> subtags
              )

      Just _type ->
        let
          subtagValuesPerTag =
            Map.fromList $
              catMaybes
                [ ("Type",) . Set.fromList <$> Map.lookup "Type" kv
             -- , ("Subtag",) . Set.fromList <$> Map.lookup "Subtag" kv
             -- , ("Description",) . Set.fromList <$> Map.lookup "Description" kv
             -- , ("Comments",) . Set.fromList <$> Map.lookup "Comments" kv
             -- , ("Deprecated",) . Set.fromList <$> Map.lookup "Deprecated" kv
                , ("Macrolanguage",) . Set.fromList <$> Map.lookup "Macrolanguage" kv
             -- , ("Preferred-Value",) . Set.fromList <$> Map.lookup "Preferred-Value" kv
                , ("Scope",) . Set.fromList <$> Map.lookup "Scope" kv
                , ("Suppress-Script",) . Set.fromList <$> Map.lookup "Suppress-Script" kv
                ]
        in
          ( allFields
          , usedConstructors
          , Map.unionWith (<>) subtagValuesPerTag valuesPerTag
          , subtags
          )

      Nothing ->
        (allFields, usedConstructors, valuesPerTag, subtags)
