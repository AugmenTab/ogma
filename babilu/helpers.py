#! python3

from asyncio import to_thread
from bs4 import BeautifulSoup
from os import makedirs, path
from urllib.request import Request, urlopen


BASE_LINK = "https://en.wikipedia.org"


CACHE_DIR = "babilu/cache"


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
    link = BASE_LINK + url
    filename = path.join(CACHE_DIR, url.replace("https://", "").replace("/", "_") + ".html")

    def fetch():
        req = Request(link)

        req.add_header('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0')
        req.add_header('Accept', 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8')
        req.add_header('Accept-Language', 'en-US,en;q=0.5')

        if path.exists(filename):
            with open(filename, "r", encoding="utf-8") as file:
                return BeautifulSoup(file.read(), "html.parser").find('table')

        else:
            with urlopen(req) as response:
                makedirs(CACHE_DIR, exist_ok=True)
                html = response.read().decode('utf-8')

                with open(filename, "w", encoding="utf-8") as file:
                    soup = BeautifulSoup(html, "html.parser").find('table')
                    file.write(str(soup))
                    return soup

    return await to_thread(fetch)


def with_prepend(spaces, prepend, name):
    return "\n" + indent(spaces) + prepend + " " + name

