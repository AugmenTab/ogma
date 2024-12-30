#! python3

from pathlib import Path
from re import IGNORECASE, split, sub
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


def __to_unicode_literal(name, txt):
    broken = split(r'[-\s]*(?:or|to)[-\s]*|[–]', txt, flags=IGNORECASE)
    literals = list(map(lambda x: f"chr 0x{x.strip()[2:]}", broken))
    return f"[ {literals[0]}..{literals[1]} ]"


def __parse_unicode_range(name, unicode_range):
    def preserve_unicode_range(txt):
        substrings = [
            "Final",
            "\xa0",
            "full",
            "[",
            "Private",
            "None",
            "CSUR",
            "Download",
            "attached",
            "Registry"
        ]

        return all(map(lambda x: x not in txt, substrings))

    def replace_extra_words(txt):
        return txt.replace(" Georgian", "").replace(" Supplement", "").replace(" Extended", "").replace('\u2009', "").replace("\"", "")

    if unicode_range is None:
        return None

    code_tags = unicode_range.find_all('code')

    if not code_tags:
        a_tags = list(map(lambda x: x.get_text().strip(), unicode_range.find_all('a')))

        if not a_tags:
            li_tags = list(map(lambda x: x.get_text().strip(), unicode_range.find_all('li')))

            if not li_tags:
                return None

            else:
                tags = li_tags
        else:
            tags = a_tags
    else:
        tags = ['–'.join(map(lambda x: f"U+{x.get_text()}", code_tags))]

    chunked = [x for xs in map(lambda x: x.split(','), tags) for x in xs]
    filtered = list(filter(preserve_unicode_range, chunked))
    return list(map(replace_extra_words, filtered))


def __parse_script_type(script_type):
    if script_type is None:
        return None

    return upper_camel_case(script_type.get_text().split())


def __process_name(name):
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


async def scrape_script(link, names, code, number, unicode_alias):
    soup = await get_soup(link)

    if soup is None:
        return {
            'constructor': __process_name(names),
            'names': None,
            'type': None,
            'axis': None,
            'direction': None,
            'parent': None,
            'endonym': None,
            'romanized': None,
            'iso-15924-code': code,
            'iso-15924-number': number,
            'unicode_alias': unicode_alias,
            'unicode_range': None
            }

    rows = soup.find('tbody').find_all('tr')
    cell_map = {}

    for row in rows:
        th = row.find('th')
        td = row.find('td')

        if th and td:
            cell_map[th.get_text(strip=True)] = td

    # script_type = __parse_script_type(cell_map.get('Script type'))
    # print(f"{names}: {script_type}")

    if names == 'Latin':
        unicode = [
            'U+0000–U+007F',
            'U+0080–U+00FF',
            'U+0100–U+017F',
            'U+0180–U+024F',
            'U+1E00–U+1EFF'
        ]

    elif "Meetei" in names:
        unicode = ['U+ABC0–U+ABFF']

    elif names == "Egyptian hieratic":
        unicode = ['U+13000–U+1342F', 'U+13460–U+143FF', 'U+13430–U+1345F']

    else:
        unicode = __parse_unicode_range(names, cell_map.get('Unicode range'))

    if unicode:
        unicode_range = list(map(lambda x: __to_unicode_literal(names, x), unicode))

    else:
        unicode_range = None

    return {
        'constructor': __process_name(names),
        'names': None,
        'type': None, # script_type,
        'axis': None,
        'direction': None,
        'parent': None,
        'endonym': None,
        'romanized': None,
        'iso-15924-code': code,
        'iso-15924-number': number,
        'unicode_alias': unicode_alias,
        'unicode_range': unicode_range
        }


async def __parse_scripts(rows):
    scripts = {}

    for row in rows:
        cells = row.find_all('td')

        if len(cells) < 3:
            continue

        code = cells[0].get_text().strip()
        if len(code) > 4:
            continue

        name_cell = cells[2]
        names = name_cell.get_text().strip()
        link = name_cell.find('a').get('href')
        number = cells[1].get_text().strip()

        if cells[4].has_attr('colspan'):
            unicode_alias = None

        else:
            unicode_alias = cells[4].get_text().strip()

        script = await scrape_script(link, names, code, number, unicode_alias)
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

