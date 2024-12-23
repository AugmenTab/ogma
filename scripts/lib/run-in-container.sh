if [ "$IN_DEV_CONTAINER" ]; then
  # Already in container, nothing to do
  :
else
  # Script was run from outside the container, re-exec inside the container
  # with the same arguments.
  docker compose --file compose.ogma.yml build
  exec docker compose --file compose.ogma.yml run --rm html $0 "$@"
fi
