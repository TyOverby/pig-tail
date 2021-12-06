#!/bin/bash
set -euo pipefail

echo "stopping..."
docker stop some-postgres > /dev/null || true
docker rm some-postgres > /dev/null || true
docker network rm some-network > /dev/null || true
echo "done!"