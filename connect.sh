#!/bin/bash
set -euo pipefail

docker run -i --rm --network some-network postgres psql -h some-postgres -U postgres