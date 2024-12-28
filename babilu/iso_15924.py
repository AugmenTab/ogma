#! python3

from pathlib import Path
from helpers import indent, with_prepend


def __write_module(scripts, val):
    open(str(Path.cwd()) + f'/src/Ogma/Internal/Script/ISO_15924_{val}.hs', 'w').close()

    with open(str(Path.cwd()) + f'/src/Ogma/Internal/Script/ISO_15924_{val}.hs', 'a') as f:
        f.write(f"module Ogma.Internal.Script.ISO_15924_{val}")
        f.write("\n" + indent(2) + f"( ISO_15924_{val}")
        f.write(with_prepend(2, ",", f"iso15924{val}FromText"))
        f.write(with_prepend(2, ",", f"iso15924{val}ToBytes"))
        f.write(with_prepend(2, ",", f"iso15924{val}ToText"))

        f.write(with_prepend(2, ",", f"scriptISO15924{val}"))
        f.write("\n" + indent(2) + ") where\n")

        f.write("\n" + "import Data.ByteString.Lazy qualified as LBS\n")
        f.write("import Data.ByteString.Lazy.Char8 qualified as LBS8\n")
        f.write("import Data.Text qualified as T\n")
        f.write("\n" + "import Ogma.Internal.Script.Script (Script (..))\n")

        f.write("\n" + f"newtype ISO_15924_{val} =\n")
        f.write(indent(2) + f"ISO_15924_{val}\n")
        f.write(indent(4) + "{ unISO_15924" + val + " :: String\n")
        f.write(indent(4) + "} deriving newtype (Eq, Show)\n")

        f.write(f"\niso15924{val}FromText :: T.Text -> Either String ISO_15924_{val}\n")
        f.write(f"iso15924{val}FromText txt =\n")
        f.write(indent(2) + "case T.toLower txt of\n")

        for script in scripts:
            f.write(f"{indent(4)}\"{script['iso-15924-' + val.lower()].lower()}\" -> ")
            f.write(f"Right $ ISO_15924_{val} \"{script['iso-15924-' + val.lower()]}\"\n")

        f.write(f"{indent(4)}_ -> Left $ \"Unknown ISO_15924_{val}: \" <> T.unpack txt\n")

        f.write(f"\niso15924{val}ToBytes :: ISO_15924_{val} -> LBS.ByteString\n")
        f.write(f"iso15924{val}ToBytes = LBS8.pack . unISO_15924{val}\n")

        f.write(f"\niso15924{val}ToText :: ISO_15924_{val} -> T.Text\n")
        f.write(f"iso15924{val}ToText = T.pack . unISO_15924{val}\n")

        f.write(f"\nscriptISO15924{val} :: Script -> ISO_15924_{val}\n")
        f.write(f"scriptISO15924{val} script =\n")
        f.write(indent(2) + "case script of")

        for script in scripts:
            f.write(f"\n{indent(4)}{script['constructor']} -> ")
            f.write(f"ISO_15924_{val} \"{script['iso-15924-' + val.lower()]}\"")


def write_code_module(scripts):
    __write_module(scripts, "Code")


def write_number_module(scripts):
    __write_module(scripts, "Number")

