#! python3

from asyncio import run, to_thread
from pathlib import Path
from re import split, sub, UNICODE
from string import capwords, punctuation
from urllib.error import HTTPError
from urllib.request import Request, urlopen

from bs4 import BeautifulSoup


def indent(n):
    return " " * n


def create_language_module(languages):
    module_comment = '''-- | TODO: Write module documentation
--
-- A complete list of the codes and their details (from which this module was
-- assembled) can be found here:
--
-- https://en.wikipedia.org/wiki/List_of_language_names
--
'''
    open(str(Path.cwd()) + '/src/Ogma/Language/Language.hs', 'w').close()

    with open(str(Path.cwd()) + '/src/Ogma/Language/Language.hs', 'a') as f:
        def write_with_prepend(spaces, prepend, name):
            f.write("\n" + indent(spaces) + prepend + " " + name)

        f.write(module_comment)
        f.write("module Ogma.Language.Language")
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

        f.write("\n" + indent(2) + "deriving stock (Eq, Show)")


def upper_camel_case(words):
    return ''.join(word.capitalize() for word in words)


def process_name(name, translation_table):
    sanitized = name.split('/')[0].translate(translation_table)
    return upper_camel_case(split(r'\s+|(?<=\w)-(?=\w)', sanitized))


async def parse_languages(rows):
    replacements = {
        'Ä': 'A',
        'ä': 'a',
        'á': 'a',
        'ā': 'a',
        'ã': 'a',
        'â': 'a',
        'è': 'e',
        'é': 'e',
        'ë': 'e',
        'í': 'i',
        'ï': 'i',
        'ñ': 'n',
        'õ': 'o',
        'ö': 'o',
        'ü': 'u',
        'Ü': 'U',
        '\'': '',
        '(': ' ',
        ')': ''
    }

    translation_table = str.maketrans(replacements)
    languages = {}

    for row in rows:
        lang_name = row.contents[0].get_text()
        # lang_page = await get_soup(row.get('href'))

        language = {
            'constructor': process_name(lang_name, translation_table),
            'name': lang_name,
            'linguonym': None,
            'romanized': None,
            'script': None,
            'dialects': None,
            'iso-639-1': None,
            'iso-639-2': None,
            'iso-639-3': None
        }

        # if language['iso-639-3'] not in languages:
        languages[language['constructor']] = language

    return sorted(list(languages.values()), key=lambda x: x['constructor'])


def get_table_rows(res):
    languages_section = res.find("div", class_="mw-heading mw-heading2")
    p_tags = []

    if languages_section:
        for sibling in languages_section.find_all_next():
            if sibling.get("class") == ["mw-heading", "mw-heading2"]:
                break
            if sibling.name == "p":
                p_tags.append(sibling.find_all('a')[0])

    return p_tags


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


async def main():
    print("Fetching languages...")
    soup = await get_soup("/wiki/List_of_language_names")
    print("Languages fetched.")

    print("Parsing languages...")
    languages = await parse_languages(get_table_rows(soup))
    print("Languages parsed..")

    print("Writing Ogma.Language.Language...")
    create_language_module(languages)
    print("Done.")
    exit(0)


if __name__ == "__main__":
    try:
        run(main())

    except HTTPError as e:
        print("HTTPError " + str(e.code) + ": " + e.reason)
        err_out()
