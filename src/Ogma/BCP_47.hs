module Ogma.BCP_47
  ( BCP_47
  , mkBCP_47
  , simpleBCP_47
  , bcp_47ToBytes
  , bcp_47ToText
  ) where

import Control.Applicative ((<|>))
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Ogma.Internal.Language.ISO_639_1 (iso_639_1ToBytes, iso_639_1ToText, languageISO_639_1)
import Ogma.Internal.Language.ISO_639_2 (iso_639_2ToBytes, iso_639_2ToText, languageISO_639_2)
import Ogma.Internal.Language.ISO_639_3 (iso_639_3ToBytes, iso_639_3ToText, languageISO_639_3)
import Ogma.Internal.Language.Language (Language)

newtype BCP_47 =
  BCP_47
    { unBCP_47 :: Language
    } deriving (Eq, Show)

-- TODO: This will represent a more complex smart constructor in the future.
-- This may eventually be changed to a Brigid-esque constraint-based
-- constructor if we're able to perform type checking to construct
-- spec-compliant BCP-47 codes at compile time.
--
mkBCP_47 :: Language -> Either String BCP_47
mkBCP_47 = Right . simpleBCP_47

-- This allows users to create extremely straightforward BCP-47 codes based
-- only on an ISO 639 language code without all the complexity that comes with
-- a full BCP-47 code.
--
simpleBCP_47 :: Language -> BCP_47
simpleBCP_47 = BCP_47

bcp_47ToBytes :: BCP_47 -> LBS.ByteString
bcp_47ToBytes (BCP_47 lang) =
  fromMaybe (iso_639_3ToBytes $ languageISO_639_3 lang) $
    fmap iso_639_1ToBytes (languageISO_639_1 lang)
      <|> fmap iso_639_2ToBytes (languageISO_639_2 lang)

bcp_47ToText :: BCP_47 -> T.Text
bcp_47ToText (BCP_47 lang) =
  fromMaybe (iso_639_3ToText $ languageISO_639_3 lang) $
    fmap iso_639_1ToText (languageISO_639_1 lang)
      <|> fmap iso_639_2ToText (languageISO_639_2 lang)
