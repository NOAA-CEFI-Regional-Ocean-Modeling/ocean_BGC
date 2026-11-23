#!/usr/bin/env bash
# Copy the outputs of a finished 1D COBALT run into one folder for the answer
# report, and record the setup the run was made in (env.txt).
#
# Usage: collect_1d_outputs.sh DEST ROLE IMAGE
#   DEST   folder to create (it must not exist yet)
#   ROLE   "base" or "pr"
#   IMAGE  container image the run used
#
# Run it from the 1D experiment folder (CEFI-regional-MOM6/exps/OM4.single_column.COBALT)
# after driver.sh. Only model outputs are copied, not the files written by the
# budget checks in the workflow.
set -euo pipefail

if [[ $# -ne 3 ]]; then
  echo "usage: $0 DEST ROLE IMAGE" >&2
  exit 2
fi
dest=$1
role=$2
image=$3
cefi=$(cd ../.. && pwd)

if [[ -e $dest ]]; then
  echo "error: $dest already exists" >&2
  exit 2
fi
mkdir -p "$dest"

shopt -s nullglob
# Restart folders written by driver.sh (they also hold ocean.stats*).
for dir in RESTART_*; do
  if [[ -d $dir ]]; then
    cp -r "$dir" "$dest/"
  fi
done
# FMS diag files start with the date (e.g. 20040101.ocean_cobalt_budget.nc).
# Also the parameter docs, the diag lists and the other outputs that driver.sh
# leaves in the run folder (checked against a real run). Not copied: logs
# (out*, err*, logfile.*, which differ from run to run) and the files written by
# the budget checks.
for file in [0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9].* *_parameter_doc.* \
  available_diags.* *.available_diags diag_manifest.yaml* seaice.stats stocks.out \
  sea_ice_geometry.nc Vertical_coordinate.nc; do
  if [[ -f $file ]]; then
    cp "$file" "$dest/"
  fi
done

# The setup of this run. compare_1d.py ignores "role" and "ocean_bgc_sha",
# treats "cpu_model" as information only, and needs every other key to match.
first_line() { "$@" 2>/dev/null | head -n 1 || true; }
{
  echo "role=$role"
  echo "image=$image"
  echo "ocean_bgc_sha=$(git -C "$cefi/src/ocean_BGC" rev-parse HEAD 2>/dev/null || echo unknown)"
  echo "cefi_sha=$(git -C "$cefi" rev-parse HEAD 2>/dev/null || echo unknown)"
  # Submodule pins, except ocean_BGC (and its mocsy), which is swapped in on purpose.
  git -C "$cefi" submodule status 2>/dev/null |
    awk '$2 != "src/ocean_BGC" { sub(/^[-+U ]/, "", $1); print "submodule." $2 "=" $1 }' || true
  echo "gfortran=$(first_line gfortran --version)"
  echo "netcdf_c=$(first_line nc-config --version)"
  echo "netcdf_fortran=$(first_line nf-config --version)"
  echo "mpi=$(first_line mpirun --version)"
  echo "glibc=$(first_line ldd --version)"
  echo "cpu_model=$(awk -F': ' '/^model name/ { print $2; exit }' /proc/cpuinfo 2>/dev/null || true)"
} > "$dest/env.txt"

echo "Collected $(find "$dest" -type f | wc -l) files into $dest"
