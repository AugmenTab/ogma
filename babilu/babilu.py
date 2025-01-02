#! python3

from asyncio import run
from urllib.error import HTTPError

from get_languages import get_languages
from get_scripts import get_scripts
import iso_15924 as iso_15924
import iso_639 as iso_639


async def main():
    scripts = await get_scripts()
    iso_15924.write_code_module(scripts)
    iso_15924.write_number_module(scripts)

    # script_types = list(set(map(lambda x: x['type'], scripts))).sort()
    # iso_15924.write_script_type_module(script_types)

    iso_15924.write_unicode_module(scripts)

    languages = await get_languages()
    iso_639.write_iso_639_1_module(languages)
    iso_639.write_iso_639_2_module(languages)
    iso_639.write_iso_639_3_module(languages)

    iso_639.write_language_name_module(languages)
    iso_639.write_language_endonym_module(languages)

    iso_639.write_language_scope_module(languages)
    iso_639.write_language_type_module(languages)

    exit(0)


if __name__ == "__main__":
    try:
        run(main())

    except HTTPError as e:
        print("HTTPError " + str(e.code) + ": " + e.reason)
        print("Exiting...")
        exit(1)

