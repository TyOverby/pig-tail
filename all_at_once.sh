#!/usr/bin/env bash

# Setup environment 
PORT=$(python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')
DATA_DIR=$(mktemp -d)
trap 'rm -rf "$DATA_DIR"' EXIT

# Initialize and start database
initdb --auth-host=trust --username=postgres "$DATA_DIR"
pg_ctl -o "-p $PORT" -D "$DATA_DIR" -l "$DATA_DIR/logfile" start
trap 'pg_ctl -D "$DATA_DIR" stop' EXIT

# Run ocaml
dune exec ./src/main.exe -- -user postgres -host localhost -port "$PORT"
