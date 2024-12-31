#! python3

from asyncio import run
from urllib.error import HTTPError

from get_languages import get_languages
from get_scripts import get_scripts
from iso_15924 import write_code_module, write_number_module, write_script_type_module, write_unicode_module
from iso_639 import write_iso_639_1_module, write_iso_639_2_module, write_iso_639_3_module


async def main():
    scripts = await get_scripts()
    write_code_module(scripts)
    write_number_module(scripts)

    # script_types = list(set(map(lambda x: x['type'], scripts))).sort()
    # write_script_type_module(script_types)

    write_unicode_module(scripts)

    languages = await get_languages()
    write_iso_639_1_module(languages)
    write_iso_639_2_module(languages)
    write_iso_639_3_module(languages)

    exit(0)


if __name__ == "__main__":
    try:
        run(main())

    except HTTPError as e:
        print("HTTPError " + str(e.code) + ": " + e.reason)
        print("Exiting...")
        exit(1)

