#! python3

from pathlib import Path
from re import split
from time import time

from helpers import get_soup, indent, with_prepend


__LANGUAGE_SCOPE = {
    'I': "Individual",
    'M': "Macrolanguage"
}


__LANGUAGE_TYPE = {
    'A': "Ancient",
    'C': "Constructed",
    'E': "Extinct",
    'H': "Historical",
    'L': "Living"
}


def __create_language_module(languages):
    module_comment = '''-- | TODO: Write module documentation
--
-- A complete list of the codes and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/List_of_ISO_639-3_codes
--
'''
    open(str(Path.cwd()) + '/src/Ogma/Internal/Language.hs', 'w').close()

    with open(str(Path.cwd()) + '/src/Ogma/Internal/Language.hs', 'a') as f:
        f.write(module_comment)
        f.write("module Ogma.Internal.Language")
        f.write(with_prepend(2, "(", "Language"))
        f.write(with_prepend(6, "(", languages[0]['constructor']))

        for language in languages[1:]:
            f.write(with_prepend(6, ",", language['constructor']))

        f.write("\n" + indent(6) + ")")
        f.write("\n" + indent(2) + ") where\n")
        f.write("\n" + "data Language")
        f.write(with_prepend(2, "=", languages[0]['constructor']))

        for language in languages[1:]:
            f.write(with_prepend(2, "|", language['constructor']))

        f.write("\n" + indent(2) + "deriving stock (Bounded, Enum, Eq, Show)")


def __chunk_words(txt):
    return split("; | or ", txt)


def __parse_scope_and_type(txt):
    if txt == '—':
        return []

    pieces = txt.split('/')

    if pieces == ['']:
        return [None, None]
    elif 'S' in pieces:
        return []
    else:
        return [__LANGUAGE_SCOPE[pieces[0]], __LANGUAGE_TYPE[pieces[1]]]


def __get_code(row):
    th = row.find('th')
    th_code = th.find('code')

    if th_code is None:
        th_a = th.find('a')

        if th_a is None:
            return row.find_all('td')[1].find('code').find('a')
        else:
            return th_a
    else:
        return th_code.find('a')


def __process_name(name):
    def not_in_outlier(text):
        return text not in [' ', '', '=']

    matches = findall(r'\(([^)]*?)\)', name)

    for match in matches:
        if any(char.isdigit() for char in match) or "specifically" in match.lower():
            name = sub(r'\(' + escape(match) + r'\)', '', name)

    replaced = name.replace("(New)", "New")
    flipped = list(map(lambda x: x.strip(), replaced.split(', ')))[::-1]

    if len(name.split(' ')) == 1:
        chunked = [''.join(' '.join(flipped).split('/'))]
    else:
        chunked = list(filter(not_in_outlier, ' '.join(flipped).split('/')))

    sanitized = chunked[0].translate(TRANSLATION_TABLE)
    return upper_camel_case(split(r'\s+|(?<=\w)-(?=\w)', sanitized))


async def __parse_languages(rows):
    languages = {}

    for row in rows:
        code = __get_code(row)

        if code is None:
            continue

        cells = row.find_all('td')
        scope_and_type = __parse_scope_and_type(cells[2].get_text())

        if scope_and_type == []:
            continue

        names = list(map(lambda x: x.strip(), __chunk_words(cells[5].get_text())))
        # lang_page = await get_soup(code.get('href'))

        language = {
            'constructor': __process_name(names[0]),
            'names': names,
            'scope': scope_and_type[0],
            'type': scope_and_type[1],
            'endonyms': None,
            'romanized': None,
            'family': None,
            'dialects': None,
            'scripts': None,
            'official_language_in': None,
            'iso-639-1': None,
            'iso-639-2': None,
            'iso-639-3': code.get_text()
        }

        languages[language['constructor']] = language

    return languages


async def get_languages():
    print("Fetching languages...\n")
    start = time()
    letters = [chr(i) for i in range(ord('a'), ord('z') + 1)]
    languages = {}

    for letter in letters:
        upper = letter.upper()

        print(f"Fetching {upper} languages...")
        start_letter = time()
        soup = await get_soup(f"/wiki/ISO_639:{letter}")
        print(f"{upper} languages fetched.")

        print(f"Parsing {upper} languages...")
        parsed = await __parse_languages(soup.find('table').find_all('tr')[2:])
        languages.update(parsed)
        end_letter = time()
        print(f"{upper} languages parsed in {end_letter - start_letter} seconds.\n")

    end = time()
    print(f"Fetched all languages in {end - start} seconds.\n")

    langs = sorted(list(languages.values()), key=lambda x: x['constructor'])

    print("Writing Ogma.Internal.Language...")
    __create_language_module(langs)
    print("Done.")

