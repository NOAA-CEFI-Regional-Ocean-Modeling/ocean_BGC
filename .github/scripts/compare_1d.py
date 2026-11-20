#!/usr/bin/env python3
"""Compare the outputs of two 1D COBALT runs and report what changed.

The ocean_BGC CI runs the 1D case twice: once with the base commit and once
with the PR. This script compares the two output folders and writes a short
report.

Usage:
    compare_1d.py BASE_DIR PR_DIR [--summary FILE] [--github-annotations]

The full report (every row) is printed, so it is in the job log. The copy
appended to --summary is shortened to MAX_ROWS rows per table.

What is compared:
    * netCDF files (found by their header bytes, so ``*.nc.0000`` files count):
      every variable, bitwise. A NaN equals a NaN with the same bits; -0 and 0
      differ. Masking and scaling are off, so fill values can't hide a change.
      netCDF groups are not used by FMS output and are not compared.
    * All other files must match byte for byte.
    * ``env.txt`` is not an output. It records where each run was made. If the
      two runs were made in a different setup, the comparison is not valid.

Verdict:
    1. Answers (model state): data in the restart files, ``ocean.stats``,
       ``ocean.stats.nc``, ``seaice.stats`` and ``stocks.out``, plus restart
       variables or files that the PR removed. Only this line counts as an
       answer change. It gives the restart files, ``ocean.stats.nc`` and the
       text files separately, and the table lists restart variables first.
    2. Diagnostics: data in the diag files. Reported, but not an answer change.
    3. Outputs added or removed: files or variables on one side only, with how
       many are in restart files.
    4. Parameters / diag list: ``*_parameter_doc.*``, ``available_diags.*``,
       ``*.available_diags`` and ``diag_manifest.yaml*``.
    5. Metadata only: attribute changes.

A side without any restart file counts as missing output (the run failed): a
failed build still leaves env.txt and the parameter docs that CEFI keeps in git.

Note: in a 48 h run, only the daily diag files hold data; the others only hold
fill values, so diagnostics that only go to those files are not really tested.
The report says this under the verdict table (DIAG_NOTE).

Exit status: 0 (report-only); 2 for a wrong command line.
"""

import argparse
import dataclasses
import difflib
import fnmatch
import re
import sys
from pathlib import Path

import netCDF4
import numpy as np

NETCDF_MAGIC = (b'CDF\x01', b'CDF\x02', b'CDF\x05', b'\x89HDF\r\n\x1a\n')
ENV_FILE = 'env.txt'
# env.txt keys that are expected to differ between the base and the PR run.
ENV_EXPECTED_DIFFERENT = frozenset({'role', 'ocean_bgc_sha'})
# env.txt keys that may differ without making the comparison invalid.
ENV_INFO_ONLY = frozenset({'cpu_model'})
PARAMETER_PATTERNS = (
    '*_parameter_doc.*',
    'available_diags.*',
    '*.available_diags',
    'diag_manifest.yaml*',
)
STATE_FILES = ('seaice.stats', 'stocks.out')
RESTART_NAME = re.compile(r'\.res(_\d+)?\.nc')
MAX_ROWS = 50
MAX_DIFF_LINES = 40
DIAG_NOTE = (
    'Note: in the 48 h CI run only the daily diag files hold data. The other diag '
    "files hold only fill values, so diagnostics that only go there aren't really "
    'tested.'
)


@dataclasses.dataclass
class VarDiff:
    """A variable whose data differs between the base and the PR run."""

    file: str
    name: str
    reason: str  # 'data', 'shape' or 'dtype'
    detail: str = ''
    n_diff: int = 0
    n_total: int = 0
    max_abs_diff: float | None = None
    first_index: list[int] | None = None


@dataclasses.dataclass
class TextDiff:
    """A non-netCDF file that differs, with a short unified diff."""

    file: str
    lines: list[str]


@dataclasses.dataclass
class Report:
    """Everything found when comparing the base and the PR output folders."""

    base_dir: str
    pr_dir: str
    missing: list[str] = dataclasses.field(default_factory=list)
    errors: list[str] = dataclasses.field(default_factory=list)
    files_compared: int = 0
    vars_compared: int = 0
    # Model state (verdict line 1).
    var_diffs: list[VarDiff] = dataclasses.field(default_factory=list)
    text_diffs: list[TextDiff] = dataclasses.field(default_factory=list)
    # Diagnostics (verdict line 2).
    diag_diffs: list[VarDiff] = dataclasses.field(default_factory=list)
    diag_text_diffs: list[TextDiff] = dataclasses.field(default_factory=list)
    param_diffs: list[TextDiff] = dataclasses.field(default_factory=list)
    added: list[str] = dataclasses.field(default_factory=list)
    removed: list[str] = dataclasses.field(default_factory=list)
    removed_restart: list[str] = dataclasses.field(default_factory=list)
    metadata: list[str] = dataclasses.field(default_factory=list)
    env_missing: list[str] = dataclasses.field(default_factory=list)
    env_mismatch: dict[str, list[str]] = dataclasses.field(default_factory=dict)
    env_info: dict[str, list[str]] = dataclasses.field(default_factory=dict)

    @property
    def answer_change(self) -> bool:
        """True if the PR changed model answers (verdict line 1)."""
        return bool(self.var_diffs or self.text_diffs or self.removed_restart)

    @property
    def diagnostics_change(self) -> bool:
        """True if diag outputs changed (verdict line 2; not an answer change)."""
        return bool(self.diag_diffs or self.diag_text_diffs)

    @property
    def valid(self) -> bool:
        """False if the two sides can't be compared fairly."""
        return not (self.missing or self.errors or self.env_mismatch)


def is_netcdf(path: Path) -> bool:
    """Return True if the file starts with a netCDF-3 or HDF5 header."""
    with path.open('rb') as stream:
        return stream.read(8).startswith(NETCDF_MAGIC)


def is_parameter_file(rel: str) -> bool:
    """Return True for parameter docs and diag lists (verdict line 4)."""
    name = Path(rel).name
    return any(fnmatch.fnmatch(name, pattern) for pattern in PARAMETER_PATTERNS)


def is_restart(rel: str) -> bool:
    """Return True for restart files, e.g. MOM.res.nc, MOM.res_1.nc, coupler.res."""
    name = Path(rel).name
    return bool(RESTART_NAME.search(name)) or name.endswith('.res')


def is_state(rel: str) -> bool:
    """Return True for files that describe the model state (verdict line 1)."""
    name = Path(rel).name
    return is_restart(rel) or name.startswith('ocean.stats') or name in STATE_FILES


def list_files(root: Path) -> dict[str, Path]:
    """Map the relative path of every file under root to its full path."""
    return {
        path.relative_to(root).as_posix(): path
        for path in sorted(root.rglob('*'))
        if path.is_file()
    }


def native(array: np.ndarray) -> np.ndarray:
    """Return the array in native byte order, so raw bytes can be compared."""
    if array.dtype.byteorder not in ('=', '|'):
        return array.astype(array.dtype.newbyteorder('='))
    return array


def differing_elements(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    """Return a boolean mask of the elements whose stored bits differ.

    Both arrays must have the same shape and dtype. Comparing raw bytes treats
    a NaN as equal to a NaN with the same bits and -0 as different from 0, which
    is what a bitwise regression check needs.
    """
    a = np.atleast_1d(a)
    b = np.atleast_1d(b)
    if a.size == 0:
        return np.zeros(a.shape, dtype=bool)
    if a.dtype.kind == 'O':  # variable-length strings
        flags = [x != y for x, y in zip(a.ravel(), b.ravel(), strict=True)]
        return np.array(flags, dtype=bool).reshape(a.shape)
    a_bytes = np.ascontiguousarray(a).view(np.uint8).reshape((*a.shape, -1))
    b_bytes = np.ascontiguousarray(b).view(np.uint8).reshape((*b.shape, -1))
    return (a_bytes != b_bytes).any(axis=-1)


def max_abs_difference(a: np.ndarray, b: np.ndarray, mask: np.ndarray) -> float | None:
    """Return the largest finite |a - b| over the masked elements, if numeric."""
    if a.dtype.kind not in 'fiu' or not mask.any():
        return None
    with np.errstate(invalid='ignore', over='ignore'):
        diff = np.abs(a[mask].astype(np.float64) - b[mask].astype(np.float64))
    finite = diff[np.isfinite(diff)]
    return float(finite.max()) if finite.size else None


def compare_variable(rel: str, name: str, var_a, var_b) -> VarDiff | None:
    """Compare one netCDF variable bitwise; return None if identical."""
    a = native(np.atleast_1d(np.asarray(var_a[...])))
    b = native(np.atleast_1d(np.asarray(var_b[...])))
    if a.shape != b.shape:
        return VarDiff(rel, name, 'shape', f'{a.shape} -> {b.shape}')
    if a.dtype != b.dtype:
        return VarDiff(rel, name, 'dtype', f'{a.dtype} -> {b.dtype}')
    mask = differing_elements(a, b)
    n_diff = int(mask.sum())
    if n_diff == 0:
        return None
    first = [int(i) for i in np.argwhere(mask)[0]] if var_a.ndim else []
    return VarDiff(
        rel,
        name,
        'data',
        n_diff=n_diff,
        n_total=int(mask.size),
        max_abs_diff=max_abs_difference(a, b, mask),
        first_index=first,
    )


def attributes(obj) -> dict:
    """Return the netCDF attributes of a dataset or variable as a dict."""
    return {key: obj.getncattr(key) for key in obj.ncattrs()}


def same_value(x, y) -> bool:
    """Return True if two attribute values are equal (NaN equals NaN)."""
    ax, ay = np.asarray(x), np.asarray(y)
    if ax.shape != ay.shape or ax.dtype.kind != ay.dtype.kind:
        return False
    if ax.dtype.kind in 'fc':
        return bool(np.array_equal(ax, ay, equal_nan=True))
    return bool(np.array_equal(ax, ay))


def compare_attributes(
    rel: str, label: str, attrs_a: dict, attrs_b: dict, skip: frozenset = frozenset()
) -> list[str]:
    """List the added, removed and changed attributes of one object."""
    changes = []
    for key in sorted((set(attrs_a) | set(attrs_b)) - skip):
        if key not in attrs_b:
            changes.append(f'{rel}: {label}: attribute {key} removed')
        elif key not in attrs_a:
            changes.append(f'{rel}: {label}: attribute {key} added')
        elif not same_value(attrs_a[key], attrs_b[key]):
            changes.append(f'{rel}: {label}: attribute {key} changed')
    return changes


def record_removed(report: Report, item: str, rel: str) -> None:
    """Record a file or variable that only the base run has."""
    report.removed.append(item)
    if is_restart(rel):
        report.removed_restart.append(item)


def compare_netcdf(rel: str, path_a: Path, path_b: Path, report: Report) -> None:
    """Compare every variable and attribute of two netCDF files."""
    with netCDF4.Dataset(path_a) as ds_a, netCDF4.Dataset(path_b) as ds_b:
        ds_a.set_auto_maskandscale(False)
        ds_b.set_auto_maskandscale(False)
        report.metadata.extend(
            compare_attributes(rel, '(global)', attributes(ds_a), attributes(ds_b))
        )
        names_a, names_b = set(ds_a.variables), set(ds_b.variables)
        for name in sorted(names_a - names_b):
            record_removed(report, f'{rel}: {name}', rel)
        report.added.extend(f'{rel}: {name}' for name in sorted(names_b - names_a))
        for name in sorted(names_a & names_b):
            var_a, var_b = ds_a.variables[name], ds_b.variables[name]
            report.vars_compared += 1
            diff = compare_variable(rel, name, var_a, var_b)
            if diff:
                target = report.var_diffs if is_state(rel) else report.diag_diffs
                target.append(diff)
            # A changed checksum attribute only repeats a data change already listed.
            skip = frozenset({'checksum'}) if diff else frozenset()
            attrs_a, attrs_b = attributes(var_a), attributes(var_b)
            changes = compare_attributes(rel, name, attrs_a, attrs_b, skip)
            report.metadata.extend(changes)


def compare_text(rel: str, path_a: Path, path_b: Path) -> TextDiff | None:
    """Compare two non-netCDF files byte for byte; return a diff if different."""
    data_a, data_b = path_a.read_bytes(), path_b.read_bytes()
    if data_a == data_b:
        return None
    lines_a = data_a.decode('utf-8', errors='replace').splitlines()
    lines_b = data_b.decode('utf-8', errors='replace').splitlines()
    diff = difflib.unified_diff(
        lines_a, lines_b, f'base/{rel}', f'pr/{rel}', n=0, lineterm=''
    )
    return TextDiff(rel, list(diff))


def read_env(path: Path) -> dict[str, str]:
    """Read the key=value lines of an env.txt file."""
    env = {}
    for line in path.read_text(encoding='utf-8', errors='replace').splitlines():
        key, sep, value = line.partition('=')
        if sep and key.strip() and not key.startswith('#'):
            env[key.strip()] = value.strip()
    return env


def compare_env(env_a: dict[str, str], env_b: dict[str, str], report: Report) -> None:
    """Record setup differences between the two runs."""
    for key in sorted((set(env_a) | set(env_b)) - ENV_EXPECTED_DIFFERENT):
        pair = [env_a.get(key, '(missing)'), env_b.get(key, '(missing)')]
        if pair[0] == pair[1]:
            continue
        if key in ENV_INFO_ONLY:
            report.env_info[key] = pair
        else:
            report.env_mismatch[key] = pair


def compare_file(rel: str, path_a: Path, path_b: Path, report: Report) -> None:
    """Compare one file that exists on both sides."""
    netcdf_a, netcdf_b = is_netcdf(path_a), is_netcdf(path_b)
    if netcdf_a and netcdf_b:
        compare_netcdf(rel, path_a, path_b, report)
        return
    if netcdf_a != netcdf_b:
        diff = TextDiff(rel, ['file type changed (netCDF vs other)'])
    else:
        diff = compare_text(rel, path_a, path_b)
    if not diff:
        return
    if is_parameter_file(rel):
        report.param_diffs.append(diff)
    elif is_state(rel):
        report.text_diffs.append(diff)
    else:
        report.diag_text_diffs.append(diff)


def compare_dirs(base_dir: Path, pr_dir: Path) -> Report:
    """Compare every file in the base and the PR output folders."""
    report = Report(str(base_dir), str(pr_dir))
    report.missing = [
        side for side, path in (('base', base_dir), ('PR', pr_dir)) if not path.is_dir()
    ]
    if report.missing:
        return report
    files_a, files_b = list_files(base_dir), list_files(pr_dir)
    # A failed run has no restart files (see the module docstring).
    report.missing = [
        side
        for side, files in (('base', files_a), ('PR', files_b))
        if not any(is_restart(rel) for rel in files)
    ]
    if report.missing:
        return report
    if ENV_FILE in files_a and ENV_FILE in files_b:
        compare_env(read_env(files_a[ENV_FILE]), read_env(files_b[ENV_FILE]), report)
    else:
        report.env_missing = [
            side
            for side, files in (('base', files_a), ('PR', files_b))
            if ENV_FILE not in files
        ]
    for rel in sorted((set(files_a) | set(files_b)) - {ENV_FILE}):
        if rel not in files_b:
            record_removed(report, rel, rel)
        elif rel not in files_a:
            report.added.append(rel)
        else:
            report.files_compared += 1
            try:
                compare_file(rel, files_a[rel], files_b[rel], report)
            except (OSError, RuntimeError, ValueError) as err:
                report.errors.append(f'{rel}: {err}')
    return report


def plural(count: int, noun: str) -> str:
    """Return '1 file' or '2 files'."""
    return f'{count} {noun}' if count == 1 else f'{count} {noun}s'


def var_key(d: VarDiff) -> tuple[str, str]:
    """Group key: file name (without folder) and variable name.

    The same variable usually differs in several RESTART folders; those are one
    row. A tracer and its stock total in ocean.stats.nc share a name but are
    different data, so the file name is part of the key.
    """
    return (Path(d.file).name, d.name)


def differ(count: int, noun: str) -> str:
    """Return '1 variable differs' or '2 variables differ'."""
    return plural(count, noun) + (' differs' if count == 1 else ' differ')


def answer_parts(report: Report) -> list[str]:
    """Describe the model state changes (verdict line 1) by kind of file."""
    parts = []
    restart = [d for d in report.var_diffs if is_restart(d.file)]
    stats = [d for d in report.var_diffs if not is_restart(d.file)]
    for label, diffs in (('restart files', restart), ('ocean.stats.nc', stats)):
        if diffs:
            n_vars = len({var_key(d) for d in diffs})
            n_files = plural(len({d.file for d in diffs}), 'file')
            parts.append(f'{label}: {differ(n_vars, "variable")} in {n_files}')
    if report.text_diffs:
        parts.append(differ(len(report.text_diffs), 'text file'))
    if report.removed_restart:
        parts.append(f'{plural(len(report.removed_restart), "restart item")} removed')
    return parts


def diagnostics_parts(report: Report) -> list[str]:
    """Describe the diagnostics changes (verdict line 2)."""
    parts = []
    if report.diag_diffs:
        n_vars = plural(len({var_key(d) for d in report.diag_diffs}), 'variable')
        n_files = plural(len({d.file for d in report.diag_diffs}), 'file')
        parts.append(f'{n_vars} with different data in {n_files}')
    if report.diag_text_diffs:
        n_text = plural(len(report.diag_text_diffs), 'other output file')
        parts.append(f'{n_text} different')
    return parts


def in_restart(items: list[str]) -> str:
    """Return ' (N in restart files)' for added or removed outputs, or ''."""
    count = sum(is_restart(item.partition(': ')[0]) for item in items)
    return f' ({count} in restart files)' if count else ''


def other_changes(report: Report) -> list[str]:
    """Describe changes outside the model state and diagnostics (lines 3-5)."""
    parts = []
    if report.param_diffs:
        n = plural(len(report.param_diffs), 'file')
        parts.append(f'parameters / diag list changed in {n}')
    if report.added:
        n_added = plural(len(report.added), 'output')
        parts.append(f'{n_added} added{in_restart(report.added)}')
    if report.removed:
        n_removed = plural(len(report.removed), 'output')
        parts.append(f'{n_removed} removed{in_restart(report.removed)}')
    if report.metadata:
        parts.append(plural(len(report.metadata), 'attribute change'))
    return parts


def headline(report: Report) -> str:
    """Return the one-line verdict shown at the top of the report."""
    if report.missing:
        sides = ' and '.join(report.missing)
        return (
            f'**No comparison:** no restart files from the {sides} '
            + ('runs. They' if len(report.missing) > 1 else 'run. That run')
            + ' probably failed; see the job log.'
        )
    if report.errors:
        unreadable = plural(len(report.errors), 'file')
        return f'**Comparison incomplete:** {unreadable} could not be read.'
    if report.env_mismatch:
        keys = ', '.join(report.env_mismatch)
        return (
            f'**Comparison not valid:** the two runs used a different setup ({keys}). '
            'Re-run the jobs.'
        )
    if report.answer_change:
        text = '**Answer change:** ' + '; '.join(answer_parts(report)) + '.'
        if report.diagnostics_change:
            text += ' Diagnostics also changed.'
    elif report.diagnostics_change:
        changes = ', '.join(diagnostics_parts(report))
        text = (
            f'**No answer change** in the model state; diagnostics changed ({changes}).'
        )
    else:
        variables = plural(report.vars_compared, 'variable')
        files = plural(report.files_compared, 'file')
        text = (
            '**No answer change:** model state and diagnostics are bitwise identical '
            f'({variables} in {files}).'
        )
    others = other_changes(report)
    if others:
        text += ' Also: ' + ', '.join(others) + '.'
    return text


def change_cell(parts: list[str], note: str = '') -> str:
    """Return the text for verdict line 1 or 2."""
    return 'CHANGED: ' + '; '.join(parts) + note if parts else 'same (bitwise)'


def environment_cell(report: Report) -> str:
    """Return the text for the environment line."""
    if report.env_mismatch:
        text = 'DIFFERENT: ' + ', '.join(report.env_mismatch)
    elif report.env_missing:
        text = 'not recorded (' + ', '.join(report.env_missing) + ')'
    else:
        text = 'same'
    if 'cpu_model' in report.env_info:
        text += '; CPU model differs'
    return text


def verdict_table(report: Report) -> list[str]:
    """Return the verdict table (five lines plus the environment)."""
    outputs = (
        f'{len(report.added)} added{in_restart(report.added)}, '
        f'{len(report.removed)} removed{in_restart(report.removed)}'
        if report.added or report.removed
        else 'none'
    )
    diag_parts = diagnostics_parts(report)
    n_params, n_meta = len(report.param_diffs), len(report.metadata)
    params = f'changed in {plural(n_params, "file")}' if n_params else 'unchanged'
    metadata = plural(n_meta, 'attribute change') if n_meta else 'none'
    return [
        '| Check | Result |',
        '|---|---|',
        f'| 1. Answers (model state) | {change_cell(answer_parts(report))} |',
        f'| 2. Diagnostics | {change_cell(diag_parts, " (not an answer change)")} |',
        f'| 3. Outputs added or removed | {outputs} |',
        f'| 4. Parameters / diag list | {params} |',
        f'| 5. Metadata only | {metadata} |',
        f'| Environment | {environment_cell(report)} |',
    ]


def details(title: str, body: list[str]) -> list[str]:
    """Wrap lines in a collapsed Markdown section."""
    return ['', '<details>', f'<summary>{title}</summary>', '', *body, '', '</details>']


def limited(items: list[str], limit: int | None) -> list[str]:
    """Return at most limit items (all if limit is None), noting the rest."""
    if limit is None or len(items) <= limit:
        return items
    return [*items[:limit], f'... and {len(items) - limit} more']


def code_block(lines: list[str], limit: int | None) -> list[str]:
    """Return lines as a fenced code block, shortened if long."""
    short = None if limit is None else MAX_DIFF_LINES
    return ['```', *limited(lines, short), '```']


def var_diff_rows(diffs: list[VarDiff], limit: int | None) -> list[str]:
    """Return a Markdown table of changed variables, one row per file name and
    variable (see var_key), listing the folders where it differs."""
    groups: dict[tuple[str, str], list[VarDiff]] = {}
    for d in diffs:
        groups.setdefault(var_key(d), []).append(d)
    rows = [
        '| Variable | File | Folders | Change | Values differing (most) '
        '| Max abs diff |',
        '|---|---|---|---|---|---|',
    ]
    # Restart variables first, then by variable name.
    order = sorted(groups, key=lambda key: (not is_restart(key[0]), key[1], key[0]))
    for file_name, name in order:
        group = groups[(file_name, name)]
        worst = max(group, key=lambda d: d.n_diff)
        count = (
            f'{worst.n_diff} of {worst.n_total}'
            if worst.reason == 'data'
            else worst.detail
        )
        values = [d.max_abs_diff for d in group if d.max_abs_diff is not None]
        max_abs = f'{max(values):.3e}' if values else ''
        folders = sorted({Path(d.file).parent.as_posix() for d in group})
        cells = [
            f'`{name}`',
            f'`{file_name}`',
            ', '.join('(top)' if f == '.' else f for f in folders),
            ', '.join(sorted({d.reason for d in group})),
            count,
            max_abs,
        ]
        rows.append('| ' + ' | '.join(cells) + ' |')
    return limited(rows, None if limit is None else limit + 2)


def detail_sections(report: Report, limit: int | None) -> list[str]:
    """Return the collapsed detail sections for everything that differs."""
    lines: list[str] = []
    if report.var_diffs:
        title = f'Changed variables ({len({var_key(d) for d in report.var_diffs})})'
        lines += details(title, var_diff_rows(report.var_diffs, limit))
    for diff in report.text_diffs:
        body = code_block(diff.lines, limit)
        lines += details(f'Changed output file: {diff.file}', body)
    if report.diag_diffs:
        n_diag = len({var_key(d) for d in report.diag_diffs})
        title = f'Changed diagnostics ({n_diag})'
        lines += details(title, var_diff_rows(report.diag_diffs, limit))
    for diff in report.diag_text_diffs:
        lines += details(
            f'Changed diag file: {diff.file}', code_block(diff.lines, limit)
        )
    if report.removed or report.added:
        body = [f'- removed: `{item}`' for item in report.removed]
        body += [f'- added: `{item}`' for item in report.added]
        lines += details('Outputs added or removed', limited(body, limit))
    for diff in report.param_diffs:
        body = code_block(diff.lines, limit)
        lines += details(f'Parameters / diag list: {diff.file}', body)
    if report.metadata:
        body = [f'- {item}' for item in report.metadata]
        lines += details('Metadata changes', limited(body, limit))
    if report.env_mismatch or report.env_info:
        body = ['| Key | Base | PR |', '|---|---|---|']
        pairs = {**report.env_mismatch, **report.env_info}
        body += [f'| {key} | {a} | {b} |' for key, (a, b) in pairs.items()]
        lines += details('Environment differences', body)
    if report.errors:
        body = [f'- {error}' for error in report.errors]
        lines += details('Files that could not be read', body)
    return lines


def render_markdown(report: Report, max_rows: int | None = MAX_ROWS) -> str:
    """Return the Markdown report; max_rows=None gives every row."""
    lines = ['## 1D answer report: base vs PR', '', headline(report), '']
    if not report.missing:
        lines += verdict_table(report)
        lines += ['', DIAG_NOTE]
        lines += detail_sections(report, max_rows)
    return '\n'.join(lines) + '\n'


def annotations(report: Report) -> list[str]:
    """Return GitHub workflow commands that make problems visible on the PR."""
    message = headline(report).replace('**', '') + ' See the job summary.'
    if not report.valid or report.answer_change:
        return [f'::warning title=1D answer report::{message}']
    if report.diagnostics_change:
        return [f'::notice title=1D answer report::{message}']
    return []


def parse_args(argv: list[str] | None) -> argparse.Namespace:
    """Parse the command line."""
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument('base_dir', type=Path, help='output folder of the base run')
    parser.add_argument('pr_dir', type=Path, help='output folder of the PR run')
    parser.add_argument(
        '--summary', type=Path, help='append the shortened Markdown report to this file'
    )
    parser.add_argument(
        '--github-annotations',
        action='store_true',
        help='print a GitHub warning (answer change, invalid) or notice (diagnostics)',
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """Run the comparison, print the full report and write the summary."""
    args = parse_args(argv)
    report = compare_dirs(args.base_dir, args.pr_dir)
    print(render_markdown(report, max_rows=None), end='')
    if args.summary:
        with args.summary.open('a', encoding='utf-8') as stream:
            stream.write(render_markdown(report))
    if args.github_annotations:
        for line in annotations(report):
            print(line)
    return 0


if __name__ == '__main__':
    sys.exit(main())
