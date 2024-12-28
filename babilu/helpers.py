#! python3

from asyncio import to_thread
from bs4 import BeautifulSoup
from urllib.request import Request, urlopen


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


def upper_camel_case(words):
    return ''.join(word.capitalize() for word in words)


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


def with_prepend(spaces, prepend, name):
    return "\n" + indent(spaces) + prepend + " " + name

