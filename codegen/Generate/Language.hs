{-# LANGUAGE OverloadedStrings #-}

module Generate.Language
  ( generateLanguageModules
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Subtags.Subtag (Subtag (..))

baseDir :: FilePath
baseDir =
  "src" </> "Ogma" </> "Language"

generateLanguageModules :: NEL.NonEmpty Subtag -> IO ()
generateLanguageModules subtags = do
  createDirectoryIfMissing True baseDir
  generateCoreLanguageModule (baseDir </> "Language.hs") subtags
  generateLanguageToNameModule (baseDir </> "LanguageName.hs") subtags
  generateLanguageFromISO639Module (baseDir </> "LanguageFromISO639.hs") subtags
  generateLanguageToISO639Module (baseDir </> "LanguageToISO639.hs") subtags

generateCoreLanguageModule :: FilePath -> NEL.NonEmpty Subtag -> IO ()
generateCoreLanguageModule fp subtags =
  let
    mkExport = mappend "      , " . subtagConstructor
    mkADT = mappend "  | " . subtagConstructor
  in
    TIO.writeFile fp
      . T.unlines
      . concat
      $ [ [ "{-# OPTIONS_GHC -O0 -fno-specialise -fno-strictness -fno-worker-wrapper #-}\n"
          ]
        , [ "module Ogma.Language.Language"
          , "  ( Language"
          , "      ( " <> subtagConstructor (NEL.head subtags)
          ]
        , mkExport <$> NEL.tail subtags
        , [ "      )"
          , "  ) where\n"
          ]
        , [ "import Data.Hashable (Hashable, hashUsing, hashWithSalt)\n"
          ]
        , [ "data Language"
          , "  = " <> subtagConstructor (NEL.head subtags)
          ]
        , mkADT <$> NEL.tail subtags
        , [ "  deriving (Bounded, Enum, Eq, Ord, Show)\n"
          ]
        , [ "instance Hashable Language where"
          , "  hashWithSalt = hashUsing fromEnum"
          ]
        ]

generateLanguageToNameModule :: FilePath -> NEL.NonEmpty Subtag -> IO ()
generateLanguageToNameModule fp subtags =
  let
    toCaseLine subtag =
      T.concat
        [ "    "
        , subtagConstructor subtag
        , " -> [Interpolate.i|"
        , NEL.head $ subtagNames subtag
        , "|]"
        ]

    caseLines =
      toCaseLine <$> NEL.toList subtags
  in
    TIO.writeFile fp
      . T.unlines
      . concat
      $ [ [ "{-# LANGUAGE QuasiQuotes #-}"
          , "{-# OPTIONS_GHC -O0 -fno-specialise -fno-strictness -fno-worker-wrapper #-}\n"
          , "module Ogma.Language.LanguageName"
          , "  ( languageToNameBytes"
          , "  , languageToNameText"
          , "  ) where\n"
          ]
        , [ "import Data.ByteString.Lazy qualified as LBS"
          , "import Data.String.Interpolate qualified as Interpolate"
          , "import Data.Text qualified as T\n"
          , "import Ogma.Language.Language (Language (..))\n"
          ]
        , [ "languageToNameBytes :: Language -> LBS.ByteString"
          , "languageToNameBytes lang ="
          , "  case lang of"
          ]
        , caseLines
        , [ "\nlanguageToNameText :: Language -> T.Text"
          , "languageToNameText lang ="
          , "  case lang of"
          ]
        , caseLines
        ]

generateLanguageFromISO639Module :: FilePath -> NEL.NonEmpty Subtag -> IO ()
generateLanguageFromISO639Module fp subtags =
  let
    toCaseLine subtag =
      T.concat
        [ "    \""
        , subtagCode subtag
        , "\" -> Right "
        , subtagConstructor subtag
        ]

    caseLines =
      toCaseLine <$> NEL.toList subtags
  in
    TIO.writeFile fp
      . T.unlines
      . concat
      $ [ [ "{-# OPTIONS_GHC -O0 -fno-specialise -fno-strictness -fno-worker-wrapper #-}\n"
          , "module Ogma.Language.LanguageFromISO639"
          , "  ( languageFromISO639Bytes"
          , "  , languageFromISO639Text"
          , "  ) where\n"
          ]
        , [ "import Data.ByteString.Lazy qualified as LBS"
          , "import Data.ByteString.Lazy.Char8 qualified as LBS8"
          , "import Data.Text qualified as T\n"
          , "import Ogma.Language.Language (Language (..))\n"
          ]
        , [ "languageFromISO639Bytes :: LBS.ByteString -> Either String Language"
          , "languageFromISO639Bytes bytes ="
          , "  case LBS8.unpack bytes of"
          ]
        , caseLines
        , [ "    code -> Left $ \"Unknown ISO 639 code: \" <> code\n"
          , "languageFromISO639Text :: T.Text -> Either String Language"
          , "languageFromISO639Text text ="
          , "  case T.unpack text of"
          ]
        , caseLines
        , [ "    code -> Left $ \"Unknown ISO 639 code: \" <> code"
          ]
        ]

generateLanguageToISO639Module :: FilePath -> NEL.NonEmpty Subtag -> IO ()
generateLanguageToISO639Module fp subtags =
  let
    toCaseLine subtag =
      T.concat
        [ "    "
        , subtagConstructor subtag
        , " -> [Interpolate.i|"
        , subtagCode subtag
        , "|]"
        ]

    caseLines =
      toCaseLine <$> NEL.toList subtags
  in
    TIO.writeFile fp
      . T.unlines
      . concat
      $ [ [ "{-# LANGUAGE QuasiQuotes #-}"
          , "{-# OPTIONS_GHC -O0 -fno-specialise -fno-strictness -fno-worker-wrapper #-}\n"
          , "module Ogma.Language.LanguageToISO639"
          , "  ( languageToISO639Bytes"
          , "  , languageToISO639Text"
          , "  ) where\n"
          ]
        , [ "import Data.ByteString.Lazy qualified as LBS"
          , "import Data.String.Interpolate qualified as Interpolate"
          , "import Data.Text qualified as T\n"
          , "import Ogma.Language.Language (Language (..))\n"
          ]
        , [ "languageToISO639Bytes :: Language -> LBS.ByteString"
          , "languageToISO639Bytes lang ="
          , "  case lang of"
          ]
        , caseLines
        , [ "\nlanguageToISO639Text :: Language -> T.Text"
          , "languageToISO639Text lang ="
          , "  case lang of"
          ]
        , caseLines
        ]
