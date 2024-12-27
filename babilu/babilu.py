#! python3

from asyncio import run
from urllib.error import HTTPError

from get_languages import get_languages


async def main():
    # scripts = await get_scripts()
    await get_languages()

    exit(0)


if __name__ == "__main__":
    try:
        run(main())

    except HTTPError as e:
        print("HTTPError " + str(e.code) + ": " + e.reason)
        print("Exiting...")
        exit(1)

