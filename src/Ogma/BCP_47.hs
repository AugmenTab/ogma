module Ogma.BCP_47
  ( BCP_47
  , mkBCP_47
  , simpleBCP_47
  , bcp_47Language
  , bcp_47ToBytes
  , bcp_47ToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Ogma.Language (Language, languageToISO639Bytes, languageToISO639Text)

newtype BCP_47 =
  BCP_47
    { bcp_47Language :: Language
    } deriving (Eq, Ord, Show)

-- TODO: This will represent a more complex smart constructor in the future.
-- This may eventually be changed to a Brigid-esque constraint-based
-- constructor if we're able to perform type checking to construct
-- spec-compliant BCP-47 codes at compile time.
--
mkBCP_47 :: Language -> Either String BCP_47
mkBCP_47 = Right . BCP_47

-- This allows users to create extremely straightforward BCP-47 codes based
-- only on an ISO 639 language code without all the complexity that comes with
-- a full BCP-47 code.
--
simpleBCP_47 :: Language -> BCP_47
simpleBCP_47 = BCP_47

bcp_47ToBytes :: BCP_47 -> LBS.ByteString
bcp_47ToBytes =
  languageToISO639Bytes . bcp_47Language

bcp_47ToText :: BCP_47 -> T.Text
bcp_47ToText =
  languageToISO639Text . bcp_47Language
