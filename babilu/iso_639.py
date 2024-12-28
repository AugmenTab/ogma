#! python3

from pathlib import Path
from helpers import indent, with_prepend


def __write_module(langs, val):
    field = 'iso-639-' + val
    maybe = any(map(lambda x: x[field] is None, langs))

    open(str(Path.cwd()) + f'/src/Ogma/Internal/Language/ISO_639_{val}.hs', 'w').close()

    with open(str(Path.cwd()) + f'/src/Ogma/Internal/Language/ISO_639_{val}.hs', 'a') as f:
        f.write(f"module Ogma.Internal.Language.ISO_639_{val}")
        f.write("\n" + indent(2) + f"( ISO_639_{val}")
        f.write(with_prepend(2, ",", f"iso639_{val}FromText"))
        f.write(with_prepend(2, ",", f"iso639_{val}ToBytes"))
        f.write(with_prepend(2, ",", f"iso639_{val}ToText"))

        f.write(with_prepend(2, ",", f"languageISO639_{val}"))
        f.write("\n" + indent(2) + ") where\n")

        f.write("\n" + "import Data.ByteString.Lazy qualified as LBS\n")
        f.write("import Data.ByteString.Lazy.Char8 qualified as LBS8\n")
        f.write("import Data.Text qualified as T\n")
        f.write("\n" + "import Ogma.Internal.Language.Language (Language (..))\n")

        f.write("\n" + f"newtype ISO_639_{val} =\n")
        f.write(indent(2) + f"ISO_639_{val}\n")
        f.write(indent(4) + "{ unISO_639_" + val + " :: String\n")
        f.write(indent(4) + "} deriving newtype (Eq, Show)\n")

        f.write(f"\niso639_{val}FromText :: T.Text -> Either String ISO_639_{val}\n")
        f.write(f"iso639_{val}FromText txt =\n")
        f.write(indent(2) + "case T.toLower txt of\n")

        for lang in langs:
            if lang[field] is None:
                continue
            else:
                f.write(f"{indent(4)}\"{lang[field]}\" -> ")
                f.write(f"Right $ ISO_639_{val} \"{lang[field]}\"\n")

        f.write(f"{indent(4)}_ -> Left $ \"Unknown ISO_639_{val}: \" <> T.unpack txt\n")

        f.write(f"\niso639_{val}ToBytes :: ISO_639_{val} -> LBS.ByteString\n")
        f.write(f"iso639_{val}ToBytes = LBS8.pack . unISO_639_{val}\n")

        f.write(f"\niso639_{val}ToText :: ISO_639_{val} -> T.Text\n")
        f.write(f"iso639_{val}ToText = T.pack . unISO_639_{val}\n")

        f.write(f"\nlanguageISO639_{val} :: Language -> ")

        if lang[field] is None:
            f.write("Maybe ")

        f.write(f"ISO_639_{val}\n")

        f.write(f"languageISO639_{val} lang =\n")
        f.write(indent(2) + "case lang of")

        for lang in langs:
            f.write(f"\n{indent(4)}{lang['constructor']} -> ")

            if maybe:
                if lang[field] is None:
                    f.write("Nothing")
                else:
                    f.write(f"Just $ ISO_639_{val} \"{lang[field]}\"")
            else:
                f.write(f"ISO_639_{val} \"{lang[field]}\"")


def write_iso_639_1_module(langs):
    __write_module(langs, "1")


def write_iso_639_2_module(langs):
    __write_module(langs, "2")


def write_iso_639_3_module(langs):
    __write_module(langs, "3")

