#! python3

from asyncio import run, to_thread
from pathlib import Path
from re import escape, findall, split, sub, UNICODE
from string import capwords, punctuation
from time import time
from urllib.error import HTTPError
from urllib.request import Request, urlopen

from bs4 import BeautifulSoup


LANGUAGE_SCOPE = {
    'I': "Individual",
    'M': "Macrolanguage"
}


LANGUAGE_TYPE = {
    'A': "Ancient",
    'C': "Constructed",
    'E': "Extinct",
    'H': "Historical",
    'L': "Living"
}


TRANSLATION_TABLE = str.maketrans({
    'Ä': 'A',
    'À': 'A',
    'Á': 'A',
    'ä': 'a',
    'à': 'a',
    'á': 'a',
    'ā': 'a',
    'ã': 'a',
    'â': 'a',
    'å': 'a',
    'ɓ': 'b',
    'ç': 'c',
    'è': 'e',
    'é': 'e',
    'ë': 'e',
    'ê': 'e',
    'ə': 'e',
    'ì': 'i',
    'í': 'i',
    'ī': 'i',
    'ï': 'i',
    'î': 'i',
    'ɨ': 'i',
    'ñ': 'n',
    'ŋ': "ng",
    'Ö': 'O',
    'ö': 'o',
    'ó': 'o',
    'õ': 'o',
    'ô': 'o',
    'ṣ': 's',
    'ṭ': 't',
    'Ü': 'U',
    'ü': 'u',
    'ù': 'u',
    'ú': 'u',
    '\'': '',
    '’': '',
    '´': '',
    '(': '_ ',
    ')': '',
    '.': '',
    '!': '',
    'ǂ': '',
    '-': ' ',
    '=': '',
    'ǁ': "ll"
})


def indent(n):
    return " " * n


def create_language_module(languages):
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
        def write_with_prepend(spaces, prepend, name):
            f.write("\n" + indent(spaces) + prepend + " " + name)

        f.write(module_comment)
        f.write("module Ogma.Internal.Language")
        write_with_prepend(2, "(", "Language")
        write_with_prepend(6, "(", languages[0]['constructor'])

        for language in languages[1:]:
            write_with_prepend(6, ",", language['constructor'])

        f.write("\n" + indent(6) + ")")
        f.write("\n" + indent(2) + ") where\n")
        f.write("\n" + "data Language")
        write_with_prepend(2, "=", languages[0]['constructor'])

        for language in languages[1:]:
            write_with_prepend(2, "|", language['constructor'])

        f.write("\n" + indent(2) + "deriving stock (Bounded, Enum, Eq, Show)")


def upper_camel_case(words):
    return ''.join(word.capitalize() for word in words)


def process_name(name):
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


def chunk_words(txt):
    return split("; | or ", txt)


def parse_scope_and_type(txt):
    if txt == '—':
        return []

    pieces = txt.split('/')

    if pieces == ['']:
        return [None, None]
    elif 'S' in pieces:
        return []
    else:
        return [LANGUAGE_SCOPE[pieces[0]], LANGUAGE_TYPE[pieces[1]]]


def get_code(row):
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


async def parse_languages(rows):
    languages = {}

    for row in rows:
        code = get_code(row)

        if code is None:
            continue

        cells = row.find_all('td')
        scope_and_type = parse_scope_and_type(cells[2].get_text())

        if scope_and_type == []:
            continue

        names = list(map(lambda x: x.strip(), chunk_words(cells[5].get_text())))
        # lang_page = await get_soup(code.get('href'))

        language = {
            'constructor': process_name(names[0]),
            'names': names,
            'scope': scope_and_type[0],
            'type': scope_and_type[1],
            'endonym': None,
            'romanized': None,
            'scripts': None,
            'dialects': None,
            'iso-639-1': None,
            'iso-639-2': None,
            'iso-639-3': code.get_text()
        }

        languages[language['constructor']] = language

    return languages


async def get_soup(url):
    req = Request("https://en.wikipedia.org" + url)

    req.add_header('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0')
    req.add_header('Accept', 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8')
    req.add_header('Accept-Language', 'en-US,en;q=0.5')

    def fetch():
        with urlopen(req) as response:
            html = response.read()
        return BeautifulSoup(html, "html.parser")

    return await to_thread(fetch)


def err_out():
    print("Exiting...")
    exit(1)


def validate_url(url):
    if not validators.url(url):
        print("Invalid URL: " + url)
        err_out()


async def get_languages():
    print("Fetching languages...")
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
        parsed = await parse_languages(soup.find('table').find_all('tr')[2:])
        languages.update(parsed)
        end_letter = time()
        print(f"{upper} languages parsed in {end_letter - start_letter} seconds.")

    end = time()
    print(f"Fetched all languages in {end - start} seconds.")
    return sorted(list(languages.values()), key=lambda x: x['constructor'])


async def main():
    # scripts = await get_scripts()
    languages = await get_languages()

    print("Writing Ogma.Internal.Language...")
    create_language_module(languages)
    print("Done.")
    exit(0)


if __name__ == "__main__":
    try:
        run(main())

    except HTTPError as e:
        print("HTTPError " + str(e.code) + ": " + e.reason)
        err_out()
