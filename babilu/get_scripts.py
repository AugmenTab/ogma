#! python3

from pathlib import Path
from re import IGNORECASE, sub
from time import time

from helpers import get_soup, indent, TRANSLATION_TABLE, upper_camel_case, with_prepend


def __create_scripts_module(scripts):
    module_comment = '''-- | TODO: Write module documentation
--
-- A complete list of the scripts and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/ISO_15924
--
'''
    open(str(Path.cwd()) + '/src/Ogma/Internal/Script/Script.hs', 'w').close()

    with open(str(Path.cwd()) + '/src/Ogma/Internal/Script/Script.hs', 'a') as f:
        f.write(module_comment)
        f.write("module Ogma.Internal.Script.Script")
        f.write(with_prepend(2, "(", "Script"))
        f.write(with_prepend(6, "(", scripts[0]['constructor']))

        for script in scripts[1:]:
            f.write(with_prepend(6, ",", script['constructor']))

        f.write("\n" + indent(6) + ")")

        f.write("\n" + indent(2) + ") where\n")
        f.write("\n" + "data Script")
        f.write(with_prepend(2, "=", scripts[0]['constructor']))

        for script in scripts[1:]:
            f.write(with_prepend(2, "|", script['constructor']))

        f.write("\n" + indent(2) + "deriving stock (Eq, Show)\n")


def process_name(name):
    def replace_parentheses(match):
        content = match.group(1)
        if "variant" in content.lower():
            scrubbed = sub(r'\bvariant\b', '', content, flags=IGNORECASE).strip()
            return f"({scrubbed}"
        else:
            return ''

    no_parens = sub(r'\(([^)]*?)\)', replace_parentheses, name).strip()
    sanitized = no_parens.translate(TRANSLATION_TABLE)
    constructor = upper_camel_case(sanitized.split(', ')[0].strip().split(' '))
    substrings = [
        "_",
        "Cursive",
        "Documents",
        "Hieroglyph",
        "Inscriptional"
        "Notation",
        "Reserved",
        "Script",
        "Shorthand",
        "Sign",
        "Speech",
        "Syllabary",
        "Syllabaries",
        "Syllabic",
        "Symbols"
    ]

    if any(substring in constructor for substring in substrings):
        return constructor
    else:
        return f"{constructor}Script"


async def __parse_scripts(rows):
    scripts = {}

    for row in rows:
        cells = row.find_all('td')

        if len(cells) < 3:
            continue

        name = cells[2].get_text().strip()
        code = cells[0].get_text().strip()

        if len(code) > 4:
            continue

        script = {
            'constructor': process_name(name),
            'names': None,
            'type': None,
            'axis': None,
            'direction': None,
            'parent': None,
            'endonym': None,
            'romanized': None,
            'iso-15924-code': code,
            'iso-15924-number': cells[1].get_text().strip(),
            'unicode_alias': None,
            'unicode_range_min': None,
            'unicode_range_max': None
        }

        scripts[script['constructor']] = script

    return scripts


async def get_scripts():
    print("Fetching scripts...")
    start = time()
    table = await get_soup('/wiki/ISO_15924')
    print("Scripts fetched.")

    print("Parsing scripts...")
    parsed = await __parse_scripts(table.find_all('tr')[3:])
    print("Scripts parsed.")

    end = time()
    print(f"Fetched all scripts in {end - start} seconds.")

    scripts = sorted(list(parsed.values()), key=lambda x: x['constructor'])

    print("Writing Ogma.Internal.Script...")
    __create_scripts_module(scripts)
    print("Done.")
    return scripts

