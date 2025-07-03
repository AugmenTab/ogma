{-# LANGUAGE OverloadedStrings #-}

module Subtags.Sanitize
  ( toConstructor
  ) where

import Data.Char (isAlphaNum, isLetter, isDigit, toUpper, isMark)
import Data.Text qualified as T
import Data.Text.Normalize (normalize, NormalizationMode(NFD))

toConstructor :: T.Text -> T.Text
toConstructor input =
  let
    (base, mbSuffix) = extractParenthetical input
    prepare =
      capitalizeWords . fmap (sanitize . stripDiacritics) . splitToWords

    baseWords = prepare base
    basePascal = T.concat baseWords
    suffixPascal =
      case prepare <$> mbSuffix of
        Just sfx ->
          if null sfx
            then T.empty
            else "_" <> T.concat sfx

        Nothing ->
          T.empty

    result = basePascal <> suffixPascal
  in
    if T.null result || not (isLetter (T.head result))
      then "_" <> result
      else result

extractParenthetical :: T.Text -> (T.Text, Maybe T.Text)
extractParenthetical txt =
  case Just . T.breakOnEnd " (" =<< T.stripSuffix ")" txt of
    Just (baseWithParen, insideRaw) ->
      let
        base = T.dropEnd 2 baseWithParen
        inside = T.strip insideRaw
      in
        ( T.strip base
        , if T.any isDigit inside
            then Nothing
            else Just inside
        )

    Nothing ->
      (txt, Nothing)

capitalizeWords :: [T.Text] -> [T.Text]
capitalizeWords =
  let
    capitalize t =
      case T.uncons t of
        Just (h, rest) -> T.cons (toUpper h) rest
        Nothing        -> T.empty
  in
    fmap capitalize

sanitize :: T.Text -> T.Text
sanitize = T.concatMap sanitizeChar

sanitizeChar :: Char -> T.Text
sanitizeChar c
  | c == 'ɛ' = "e"
  | c == 'ə' = "e"
  | c == 'ɓ' = "b"
  | c == 'ɨ' = "i"
  | c == 'ŋ' = "ng"
  | otherwise =  T.singleton c

stripDiacritics :: T.Text -> T.Text
stripDiacritics = T.filter (not . isMark) . normalize NFD

splitToWords :: T.Text -> [T.Text]
splitToWords = T.words . T.map (\c -> if isSeparator c then ' ' else c)

isSeparator :: Char -> Bool
isSeparator c =
  not (isAlphaNum c) || c `elem` [ 'ʼ', 'ǃ', 'ǂ', 'ǁ', 'ǀ' ]
