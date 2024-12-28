#! python3

from asyncio import run
from urllib.error import HTTPError

from get_languages import get_languages
from get_scripts import get_scripts
from iso_15924 import write_code_module, write_number_module


async def main():
    scripts = await get_scripts()
    write_code_module(scripts)
    write_number_module(scripts)

    # await get_languages()

    exit(0)


if __name__ == "__main__":
    try:
        run(main())

    except HTTPError as e:
        print("HTTPError " + str(e.code) + ": " + e.reason)
        print("Exiting...")
        exit(1)

