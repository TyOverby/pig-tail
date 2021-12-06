#!/bin/bash
set -euo pipefail

echo "starting..."
docker network create -d bridge some-network > /dev/null
docker run --network some-network --name some-postgres -e POSTGRES_HOST_AUTH_METHOD=trust -d postgres > /dev/null
echo "done!"