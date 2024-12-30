#! python3

from pathlib import Path
from helpers import indent, with_prepend


def write_unicode_range(f, maybe, unicode_range):
    if maybe:
        if unicode_range is None:
            f.write(indent(6) + "Nothing")

        elif len(unicode_range) == 1:
            f.write(f"{indent(6)}Just $ UnicodeRange {unicode_range[0]}")

        else:
            f.write(indent(6) + "Just")
            f.write(with_prepend(8, ".", "UnicodeRange"))
            f.write(with_prepend(8, ".", "concat"))
            f.write(with_prepend(8, "$", "[ " + unicode_range[0]))

            for unicode_range in unicode_range[1:]:
                f.write(with_prepend(10, ",", unicode_range))

            f.write("\n" + indent(10) + "]")

    else:
        if len(unicode_range) == 1:
            f.write(f"{indent(6)}UnicodeRange {unicode_range}")

        else:
            f.write("wut")


def write_unicode_module(scripts):
    maybe = any(map(lambda x: x['unicode_range'] is None, scripts))
    module_comment = '''-- | TODO: Write module documentation
--
-- A complete list of the scripts and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/ISO_15924
--
'''
    open(str(Path.cwd()) + '/src/Ogma/Internal/Script/UnicodeRange.hs', 'w').close()

    with open(str(Path.cwd()) + '/src/Ogma/Internal/Script/UnicodeRange.hs', 'a') as f:
        f.write(module_comment)
        f.write(f"module Ogma.Internal.Script.UnicodeRange")
        f.write(with_prepend(2, "(", "UnicodeRange (..)"))
        f.write(with_prepend(2, ",", "scriptUnicodeRange"))
        f.write("\n" + indent(2) + ") where\n")

        f.write("\n" + "import Data.Char (chr)\n")
        f.write("\n" + "import Ogma.Internal.Script.Script (Script (..))\n")

        f.write("\n" + "newtype UnicodeRange = UnicodeRange [Char]\n")
        f.write(indent(2) + "deriving newtype (Eq, Show)\n")

        f.write(f"\nscriptUnicodeRange :: Script -> ")

        if maybe:
            f.write("Maybe ")

        f.write(f"UnicodeRange\n")

        f.write(f"scriptUnicodeRange script =\n")
        f.write(indent(2) + "case script of")
        f.write(f"\n{indent(4)}{scripts[0]['constructor']} ->\n")
        write_unicode_range(f, maybe, scripts[0]['unicode_range'])

        for script in scripts[1:]:
            f.write(f"\n\n{indent(4)}{script['constructor']} ->\n")
            write_unicode_range(f, maybe, script['unicode_range'])


def write_script_type_module(scripts):
    module_comment = '''-- | TODO: Write module documentation
--
-- A complete list of the scripts and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/ISO_15924
--
'''
    open(str(Path.cwd()) + '/src/Ogma/Internal/Script/ScriptType.hs', 'w').close()

    with open(str(Path.cwd()) + '/src/Ogma/Internal/Script/ScriptType.hs', 'a') as f:
        f.write(module_comment)
        f.write(f"module Ogma.Internal.Script.ScriptType")
        f.write(with_prepend(2, "(", "ScriptType"))
        f.write(with_prepend(6, "(", scripts[0]))

        for script in scripts:
            f.write(with_prepend(6, ",", script))

        f.write("\n" + indent(6) + ")")

        f.write("\n" + indent(2) + ") where\n")
        f.write("\n" + "data ScriptType")
        f.write(with_prepend(2, "=", scripts[0]))

        for script in scripts[1:]:
            f.write(with_prepend(2, "|", script))

        f.write("\n" + indent(2) + "deriving stock (Eq, Show)\n")

def __write_module(scripts, val):
    module_comment = '''-- | TODO: Write module documentation
--
-- A complete list of the scripts and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/ISO_15924
--
'''
    open(str(Path.cwd()) + f'/src/Ogma/Internal/Script/ISO_15924_{val}.hs', 'w').close()

    with open(str(Path.cwd()) + f'/src/Ogma/Internal/Script/ISO_15924_{val}.hs', 'a') as f:
        f.write(module_comment)
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

