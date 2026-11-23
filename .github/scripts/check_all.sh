#!/usr/bin/env bash
# Run every check for the CI helper scripts in this folder.
#
# Usage: .github/scripts/check_all.sh
#
# With uv: runs ruff (lint + format), the unit tests, actionlint on the
# workflows and shellcheck on the shell scripts, all with the versions pinned
# in .github/scripts/uv.lock.
# Without uv: runs only the unit tests, with $PYTHON (default python3), which
# needs netCDF4 and numpy.
set -uo pipefail

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
status=0

run() {
  echo "== $*"
  if ! "$@"; then
    echo "FAILED: $*"
    status=1
  fi
}

if command -v uv > /dev/null; then
  uvrun=(uv run --locked --project "$here")
  run "${uvrun[@]}" ruff check "$here"
  run "${uvrun[@]}" ruff format --check "$here"
  run "${uvrun[@]}" python -m pytest -q "$here/tests"
  # -shellcheck= : don't shellcheck the run: steps here. Some existing steps
  # in cobalt_ci.yml would fail it (unquoted variables); fixing them is a
  # separate change. Our own shell scripts are checked on the next line.
  run "${uvrun[@]}" actionlint -shellcheck= \
    "$repo"/.github/workflows/*.yml "$repo"/.github/workflows/*.yaml
  run "${uvrun[@]}" shellcheck "$here"/*.sh
else
  python=${PYTHON:-python3}
  echo "uv not found: running only the unit tests with $python (no lint)"
  run "$python" -m unittest discover -s "$here/tests" -v
fi

if [[ $status -eq 0 ]]; then
  echo "All checks passed."
else
  echo "Some checks FAILED."
fi
exit $status
