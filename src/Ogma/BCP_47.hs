module Ogma.BCP_47
  ( mkBCP_47
  , simpleBCP_47
  ) where

-- TODO: This will represent a more complex smart constructor in the future.
-- This may eventually be changed to a Brigid-esque constraint-based
-- constructor if we're able to perform type checking to construct
-- spec-compliant BCP-47 codes at compile time.
--
mkBCP_47 :: Either String String
mkBCP_47 = undefined

-- This allows users to create extremely straightforward BCP-47 codes based
-- only on an ISO 639 language code without all the complexity that comes with
-- a full BCP-47 code.
--
simpleBCP_47 :: String
simpleBCP_47 = undefined
