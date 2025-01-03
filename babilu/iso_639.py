#! python3

from pathlib import Path
from helpers import indent, with_prepend


def __write_simple_language_module(languages, field):
    field_name = field.capitalize()
    maybe = any(map(lambda x: x[field] is None, languages))
    module_comment = '''-- | TODO: Write module documentation
--
-- A complete list of the codes and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/List_of_ISO_639-3_codes
--
'''
    open(str(Path.cwd()) + f'/src/Ogma/Internal/Language/{field_name}.hs', 'w').close()

    with open(str(Path.cwd()) + f'/src/Ogma/Internal/Language/{field_name}.hs', 'a') as f:
        f.write(module_comment)
        f.write(f"module Ogma.Internal.Language.{field_name}")
        f.write(with_prepend(2, "(", f"language{field_name}ToBytes"))
        f.write(with_prepend(2, ",", f"language{field_name}ToText"))
        f.write("\n" + indent(2) + ") where\n")

        f.write("\n" + "import Data.ByteString.Lazy qualified as LBS\n")
        f.write("import Data.Text qualified as T\n")
        f.write("\n" + "import Ogma.Internal.Language.Language (Language (..))\n")

        f.write(f"\nlanguage{field_name}ToBytes :: Language -> ")

        if maybe:
            f.write("Maybe ")

        f.write("LBS.ByteString")
        f.write(f"\nlanguage{field_name}ToBytes {field} =")
        f.write(f"\n{indent(2)}case {field} of")

        for language in languages:
            f.write(f'\n{indent(4)}{language["constructor"]} -> ')

            if maybe:
                if language[field] is None:
                    f.write("Nothing")

                else:
                    f.write(f'Just "{language[field]}"')

            else:
                f.write(f'"{language[field]}"')

        f.write(f"\n\nlanguage{field_name}ToText :: Language -> ")

        if maybe:
            f.write("Maybe ")

        f.write("T.Text")
        f.write(f"\nlanguage{field_name}ToText {field} =")
        f.write(f"\n{indent(2)}case {field} of")

        for language in languages:
            f.write(f'\n{indent(4)}{language["constructor"]} -> ')

            if maybe:
                if language[field] is None:
                    f.write("Nothing")

                else:
                    f.write(f'Just "{language[field]}"')

            else:
                f.write(f'"{language[field]}"')


def __write_language_scope_and_type_module(languages, field):
    field_arg = "type_" if field == "type" else field
    field_name = field.capitalize()
    filtered = (filter(lambda x: x[field] is not None, languages))
    vals = sorted(list(set(map(lambda x: x[field], filtered))))
    maybe = any(map(lambda x: x[field] is None, languages))
    module_comment = '''-- | TODO: Write module documentation
--
-- A complete list of the codes and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/List_of_ISO_639-3_codes
--
'''
    open(str(Path.cwd()) + f'/src/Ogma/Internal/Language/{field_name}.hs', 'w').close()

    with open(str(Path.cwd()) + f'/src/Ogma/Internal/Language/{field_name}.hs', 'a') as f:
        f.write(module_comment)
        f.write(f"module Ogma.Internal.Language.{field_name}")
        f.write(with_prepend(2, "(", f"Language{field_name}"))
        f.write(with_prepend(6, "(", vals[0]))

        for val in vals[1:]:
            f.write(with_prepend(6, ",", val))

        f.write("\n" + indent(6) + ")")
        f.write(with_prepend(2, ",", f"language{field_name}ToBytes"))
        f.write(with_prepend(2, ",", f"language{field_name}ToText"))
        f.write(with_prepend(2, ",", f"language{field_name}"))
        f.write("\n" + indent(2) + ") where\n")

        f.write("\n" + "import Data.ByteString.Lazy qualified as LBS\n")
        f.write("import Data.Text qualified as T\n")
        f.write("\n" + "import Ogma.Internal.Language.Language (Language (..))\n")

        f.write("\n" + f"data Language{field_name}")
        f.write(with_prepend(2, "=", vals[0]))

        for val in vals[1:]:
            f.write(with_prepend(2, "|", val))

        f.write("\n" + indent(2) + "deriving stock (Bounded, Enum, Eq, Show)")

        f.write(f"\n\nlanguage{field_name}ToBytes :: Language{field_name} -> LBS.ByteString")
        f.write(f"\nlanguage{field_name}ToBytes {field_arg} =")
        f.write(f"\n{indent(2)}case {field_arg} of")

        for val in vals:
            f.write(f'\n{indent(4)}{val} -> "{val}"')

        f.write(f"\n\nlanguage{field_name}ToText :: Language{field_name} -> T.Text")
        f.write(f"\nlanguage{field_name}ToText {field_arg} =")
        f.write(f"\n{indent(2)}case {field_arg} of")

        for val in vals:
            f.write(f'\n{indent(4)}{val} -> "{val}"')

        f.write(f"\n\nlanguage{field_name} :: Language -> ")

        if maybe:
            f.write("Maybe ")

        f.write(f"Language{field_name}")
        f.write(f"\nlanguage{field_name} language =")
        f.write(f"\n{indent(2)}case language of")

        for language in languages:
            f.write(f'\n{indent(4)}{language["constructor"]} -> ')

            if maybe:
                if language[field] is None:
                    f.write("Nothing")

                else:
                    f.write(f"Just {language[field]}")

            else:
                f.write(language[field])


def __unique_codes(langs, field):
    return list(set(filter(lambda x: x is not None, map(lambda x: x[field], langs))))


def __write_module(langs, val):
    field = 'iso-639-' + val
    maybe = any(map(lambda x: x[field] is None, langs))

    open(str(Path.cwd()) + f'/src/Ogma/Internal/Language/ISO_639_{val}.hs', 'w').close()

    with open(str(Path.cwd()) + f'/src/Ogma/Internal/Language/ISO_639_{val}.hs', 'a') as f:
        f.write(f"module Ogma.Internal.Language.ISO_639_{val}")
        f.write("\n" + indent(2) + f"( ISO_639_{val}")
        f.write(with_prepend(2, ",", f"iso_639_{val}FromText"))
        f.write(with_prepend(2, ",", f"iso_639_{val}ToBytes"))
        f.write(with_prepend(2, ",", f"iso_639_{val}ToText"))

        f.write(with_prepend(2, ",", f"languageISO_639_{val}"))
        f.write("\n" + indent(2) + ") where\n")

        f.write("\n" + "import Data.ByteString.Lazy qualified as LBS\n")
        f.write("import Data.ByteString.Lazy.Char8 qualified as LBS8\n")
        f.write("import Data.Text qualified as T\n")
        f.write("\n" + "import Ogma.Internal.Language.Language (Language (..))\n")

        f.write("\n" + f"newtype ISO_639_{val} =\n")
        f.write(indent(2) + f"ISO_639_{val}\n")
        f.write(indent(4) + "{ unISO_639_" + val + " :: String\n")
        f.write(indent(4) + "} deriving newtype (Eq, Show)\n")

        f.write(f"\niso_639_{val}FromText :: T.Text -> Either String ISO_639_{val}\n")
        f.write(f"iso_639_{val}FromText txt =\n")
        f.write(indent(2) + "case T.toLower txt of\n")

        for lang in sorted(__unique_codes(langs, field)):
            f.write(f"{indent(4)}\"{lang}\" -> ")
            f.write(f"Right $ ISO_639_{val} \"{lang}\"\n")

        f.write(f"{indent(4)}_ -> Left $ \"Unknown ISO_639_{val}: \" <> T.unpack txt\n")

        f.write(f"\niso_639_{val}ToBytes :: ISO_639_{val} -> LBS.ByteString\n")
        f.write(f"iso_639_{val}ToBytes = LBS8.pack . unISO_639_{val}\n")

        f.write(f"\niso_639_{val}ToText :: ISO_639_{val} -> T.Text\n")
        f.write(f"iso_639_{val}ToText = T.pack . unISO_639_{val}\n")

        f.write(f"\nlanguageISO_639_{val} :: Language -> ")

        if maybe:
            f.write("Maybe ")

        f.write(f"ISO_639_{val}\n")

        f.write(f"languageISO_639_{val} lang =\n")
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


def write_language_name_module(langs):
    __write_simple_language_module(langs, "name")


def write_language_endonym_module(langs):
    __write_simple_language_module(langs, "endonym")


def write_language_scope_module(langs):
    __write_language_scope_and_type_module(langs, "scope")


def write_language_type_module(langs):
    __write_language_scope_and_type_module(langs, "type")

