#!/usr/bin/env bash

DATA_DIR=$(mktemp -d)
PORT=$(python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')
initdb --auth-host=trust --username=postgres "$DATA_DIR"
pg_ctl -o "-p $PORT" -D "$DATA_DIR" -l "$DATA_DIR/logfile" start
dune exec ./src/main.exe -- -user postgres -host localhost -port "$PORT"
pg_ctl -o "-p $PORT" -D "$DATA_DIR" stop

