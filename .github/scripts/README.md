# CI helper scripts

Scripts used by `.github/workflows/cobalt_ci.yml`. Maintainer: Yi-Cheng Teng (GitHub: yichengt900).

## 1D answer report

For every pull request, the CI runs the 1D COBALT case (BATS column, 48 h, from CEFI-regional-MOM6 `main`) twice: once
with the PR's ocean_BGC (job `run-obgc_1d-ci`) and once with its base commit (job `base-1d`). The job `answer-report`
then compares the two runs and writes the result to the job summary page. A warning is shown on the PR's checks page
if the answers changed or the two runs can't be compared, and a notice if only diagnostics changed.

The report has five lines:

1. **Answers (model state):** the data of every variable present in both runs is compared bitwise in the restart
   files (all tracers), `ocean.stats`, `ocean.stats.nc`, `seaice.stats` and `stocks.out`. A restart variable or
   file that the PR removed also counts. Only this line is an "answer change". It counts the restart files,
   `ocean.stats.nc` and the text files separately, and the table of changed variables lists restart variables first.
2. **Diagnostics:** data changes in the diag files. They are reported, for example after a fix to a diagnostic's
   formula, but are not an answer change.
3. **Outputs added or removed:** files or variables that only one run has, for example a new diagnostic. It also
   says how many of them are in restart files.
4. **Parameters / diag list:** changes in `*_parameter_doc.*`, `available_diags.*`, `*.available_diags` and
   `diag_manifest.yaml*`, for example a new parameter.
5. **Metadata only:** attribute changes, for example units.

It also checks that both runs used the same CEFI commit, submodule pins and compiler and library versions (`env.txt`).
If they didn't, the comparison is marked not valid.

**What it doesn't cover:** code that the 1D case doesn't run. That includes 3D configurations, land points, other
processor layouts, options that are off by default, and long runs. Those still need the Gaea tests. Also, in a 48 h run
only the daily diag files hold data. The other diag files (monthly, annual) only hold fill values, so diagnostics that
only go to those files aren't really tested (they still count in the number of variables compared). The report says
this under the verdict table.

If a run failed (no restart files), the report says there is nothing to compare and shows a warning.

**Report-only:** the compare step always exits 0, and `answer-report` is not a required check, so it never blocks a
merge. The job summary shortens long tables; the job log of
`answer-report` has every row. Today the text check of `ocean.stats` against
`.github/ref/OM4.single_column.COBALT.p4/docker-linux-gnu/ocean.stats` still decides whether the 1D answers may change.

## Files

| File | Purpose |
|---|---|
| `compare_1d.py` | compares two output folders and writes the report (`--help` for options) |
| `collect_1d_outputs.sh` | copies the outputs of a 1D run into one folder and writes `env.txt` |
| `check_all.sh` | runs every check for these scripts (see below) |
| `tests/test_compare_1d.py` | unit tests for `compare_1d.py` |
| `pyproject.toml`, `uv.lock` | pinned packages: netCDF4, numpy; ruff, pytest, actionlint, shellcheck |

## Run it yourself

```console
# Compare two output folders (for example two RESTART folders from your own runs)
uv run --project .github/scripts python .github/scripts/compare_1d.py BASE_DIR PR_DIR

# Run all checks (lint, format, unit tests, workflow and shell checks)
.github/scripts/check_all.sh
```

`check_all.sh` runs actionlint without shellcheck on the workflow `run:` steps, because some existing steps don't
pass it; the shell scripts here are checked with shellcheck.

Without uv, any Python 3.11+ with netCDF4 and numpy works:
`python .github/scripts/compare_1d.py BASE_DIR PR_DIR` and
`PYTHON=python .github/scripts/check_all.sh` (unit tests only).

## When you change these scripts

- Add a unit test for every bug you fix, before the fix.
- Run `.github/scripts/check_all.sh`.
- After changing Python packages, run `uv lock --project .github/scripts`.
