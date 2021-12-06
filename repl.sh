#!/bin/bash
set -euo pipefail

docker run -it --rm --network some-network postgres psql -h some-postgres -U postgres