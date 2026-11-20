"""Unit tests for compare_1d.py.

They use small generated files, so they run in seconds. Run them with
``python -m pytest .github/scripts/tests`` or
``python -m unittest discover -s .github/scripts/tests``.
"""

import contextlib
import importlib.util
import io
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import netCDF4
import numpy as np

SCRIPT = Path(__file__).resolve().parents[1] / 'compare_1d.py'
_spec = importlib.util.spec_from_file_location('compare_1d', SCRIPT)
compare_1d = importlib.util.module_from_spec(_spec)
sys.modules['compare_1d'] = compare_1d
_spec.loader.exec_module(compare_1d)

STATE = 'RESTART_48hrs/MOM.res.nc'  # model state (restart file)
RESTART_MARKER = 'RESTART_48hrs/coupler.res'
DIAG = '20040101.ocean_daily.nc'  # diagnostics
ENV = 'role={role}\nocean_bgc_sha={sha}\ncefi_sha=abc123\ngfortran=GNU 13.2\n'


def write_nc(path, variables, fmt='NETCDF4_CLASSIC', global_attrs=None, var_attrs=None):
    """Write a small netCDF file; dimensions are named after their sizes."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with netCDF4.Dataset(path, 'w', format=fmt) as ds:
        for key, value in (global_attrs or {}).items():
            ds.setncattr(key, value)
        for name, values in variables.items():
            data = np.asarray(values)
            dims = []
            for size in data.shape:
                dim = f'n{size}'
                if dim not in ds.dimensions:
                    ds.createDimension(dim, size)
                dims.append(dim)
            var = ds.createVariable(name, data.dtype, tuple(dims))
            for key, value in (var_attrs or {}).get(name, {}).items():
                var.setncattr(key, value)
            var[...] = data


def write_text(path, text):
    """Write a small text file."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding='utf-8')


class CompareTestCase(unittest.TestCase):
    """Base class: a base and a PR folder in a temporary directory."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.base = Path(tmp.name) / 'base'
        self.pr = Path(tmp.name) / 'pr'
        self.base.mkdir()
        self.pr.mkdir()
        write_text(self.base / 'env.txt', ENV.format(role='base', sha='aaa'))
        write_text(self.pr / 'env.txt', ENV.format(role='pr', sha='bbb'))
        # A finished run always has restart files; failed runs are tested below.
        for side in (self.base, self.pr):
            write_text(side / RESTART_MARKER, '2004 1 3 0 0 0\n')

    def both(self, rel, variables, **kwargs):
        """Write the same netCDF file on both sides."""
        write_nc(self.base / rel, variables, **kwargs)
        write_nc(self.pr / rel, variables, **kwargs)

    def compare(self):
        return compare_1d.compare_dirs(self.base, self.pr)

    def run_main_on_report(self, report):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            status = compare_1d.main(
                [report.base_dir, report.pr_dir, '--github-annotations']
            )
        return status, out.getvalue()


class TestIdentical(CompareTestCase):
    def test_identical_outputs(self):
        temp = np.linspace(1.0, 2.0, 12).reshape(3, 4)
        self.both('RESTART_48hrs/MOM.res.nc', {'Temp': temp, 'dic': temp * 2})
        self.both('20040101.ocean_daily.nc', {'chl': temp[0]})
        for side in (self.base, self.pr):
            write_text(side / 'RESTART_48hrs/ocean.stats', 'step 1 En 1.0\n')
        report = self.compare()
        self.assertFalse(report.answer_change)
        self.assertTrue(report.valid)
        self.assertEqual(report.files_compared, 4)
        self.assertEqual(report.vars_compared, 3)
        self.assertIn('No answer change', compare_1d.headline(report))

    def test_nan_in_same_place_is_equal(self):
        self.both('a.nc', {'o2': np.array([1.0, np.nan, 3.0])})
        self.assertFalse(self.compare().answer_change)

    def test_classic_and_netcdf4_formats_with_same_data(self):
        data = {'alk': np.array([2300.5, 2310.25]), 'nlev': np.arange(2, dtype='i4')}
        write_nc(self.base / 'a.nc', data, fmt='NETCDF3_64BIT_OFFSET')
        write_nc(self.pr / 'a.nc', data, fmt='NETCDF4')
        report = self.compare()
        self.assertFalse(report.answer_change, report.var_diffs)

    def test_byte_order_does_not_matter(self):
        big = np.array([1.5, -2.0], dtype='>f8')
        little = np.array([1.5, -2.0], dtype='<f8')
        self.assertIsNone(compare_1d.compare_variable('a.nc', 't', big, little))

    def test_nan_attribute_on_both_sides_is_not_a_change(self):
        attrs = {'chl': {'valid_max': np.nan}}
        self.both('a.nc', {'chl': [1.0]}, var_attrs=attrs)
        self.assertEqual(self.compare().metadata, [])


class TestAnswerChanges(CompareTestCase):
    def test_one_changed_value(self):
        base = np.zeros((2, 3))
        pr = base.copy()
        pr[1, 2] = 1.0e-12
        write_nc(self.base / 'RESTART_48hrs/MOM.res.nc', {'dic': base})
        write_nc(self.pr / 'RESTART_48hrs/MOM.res.nc', {'dic': pr})
        report = self.compare()
        self.assertTrue(report.answer_change)
        [diff] = report.var_diffs
        found = (diff.name, diff.reason, diff.n_diff, diff.n_total)
        self.assertEqual(found, ('dic', 'data', 1, 6))
        self.assertEqual(diff.first_index, [1, 2])
        self.assertAlmostEqual(diff.max_abs_diff, 1.0e-12)
        self.assertIn('Answer change', compare_1d.headline(report))

    def test_last_bit_change_is_found(self):
        value = np.array([0.1])
        write_nc(self.base / STATE, {'po4': value})
        write_nc(self.pr / STATE, {'po4': np.nextafter(value, 1.0)})
        self.assertTrue(self.compare().answer_change)

    def test_negative_zero_differs_from_zero(self):
        write_nc(self.base / STATE, {'fed': np.array([0.0])})
        write_nc(self.pr / STATE, {'fed': np.array([-0.0])})
        self.assertTrue(self.compare().answer_change)

    def test_nan_versus_number_differs(self):
        write_nc(self.base / STATE, {'o2': np.array([1.0, 2.0])})
        write_nc(self.pr / STATE, {'o2': np.array([1.0, np.nan])})
        report = self.compare()
        [diff] = report.var_diffs
        self.assertEqual(diff.n_diff, 1)
        self.assertIsNone(diff.max_abs_diff)

    def test_packed_values_are_compared_as_stored(self):
        # Scaling is off: stored 2 * 0.5 and stored 4 * 0.25 are the same value,
        # but the stored data differ, so this is a data change. (netCDF4 skips
        # scaling when scale_factor is 1.0, so both factors differ from 1.)
        for side, stored, scale in ((self.base, 2, 0.5), (self.pr, 4, 0.25)):
            with netCDF4.Dataset(side / DIAG, 'w') as ds:
                ds.createDimension('n', 1)
                var = ds.createVariable('p', 'i2', ('n',))
                var.set_auto_maskandscale(False)
                var.scale_factor = scale
                var[:] = np.array([stored], dtype='i2')
        report = self.compare()
        self.assertEqual([d.name for d in report.diag_diffs], ['p'])

    def test_dtype_change(self):
        write_nc(self.base / STATE, {'nh4': np.array([1.0], dtype='f8')})
        write_nc(self.pr / STATE, {'nh4': np.array([1.0], dtype='f4')})
        [diff] = self.compare().var_diffs
        self.assertEqual(diff.reason, 'dtype')

    def test_shape_change(self):
        write_nc(self.base / STATE, {'no3': np.zeros(3)})
        write_nc(self.pr / STATE, {'no3': np.zeros(4)})
        [diff] = self.compare().var_diffs
        self.assertEqual(diff.reason, 'shape')

    def test_text_output_change(self):
        write_text(self.base / 'RESTART_48hrs/ocean.stats', 'En 1.0\n')
        write_text(self.pr / 'RESTART_48hrs/ocean.stats', 'En 1.1\n')
        report = self.compare()
        self.assertTrue(report.answer_change)
        [diff] = report.text_diffs
        self.assertIn('+En 1.1', diff.lines)

    def test_removed_restart_variable_is_an_answer_change(self):
        rel = 'RESTART_48hrs/MOM.res.nc'
        write_nc(self.base / rel, {'Temp': [1.0], 'co3_ion': [2.0]})
        write_nc(self.pr / rel, {'Temp': [1.0]})
        report = self.compare()
        self.assertEqual(report.removed_restart, ['RESTART_48hrs/MOM.res.nc: co3_ion'])
        self.assertTrue(report.answer_change)

    def test_removed_stock_in_ocean_stats_nc_is_not_a_restart_item(self):
        rel = 'RESTART_48hrs/ocean.stats.nc'
        write_nc(self.base / rel, {'dic': [1.0], 'cased': [2.0]})
        write_nc(self.pr / rel, {'dic': [1.0]})
        report = self.compare()
        self.assertEqual(report.removed, ['RESTART_48hrs/ocean.stats.nc: cased'])
        self.assertEqual(report.removed_restart, [])
        self.assertFalse(report.answer_change)

    def test_stocks_and_seaice_stats_are_model_state(self):
        for name in ('stocks.out', 'seaice.stats'):
            write_text(self.base / name, 'S 1.0\n')
            write_text(self.pr / name, 'S 1.5\n')
        report = self.compare()
        self.assertEqual(
            sorted(d.file for d in report.text_diffs), ['seaice.stats', 'stocks.out']
        )
        self.assertTrue(report.answer_change)

    def test_diag_manifest_is_a_diag_list(self):
        write_text(self.base / 'diag_manifest.yaml.0', 'fields: [chl]\n')
        write_text(self.pr / 'diag_manifest.yaml.0', 'fields: [chl, new]\n')
        report = self.compare()
        self.assertEqual([d.file for d in report.param_diffs], ['diag_manifest.yaml.0'])
        self.assertFalse(report.answer_change)

    def test_removed_distributed_restart_file_is_an_answer_change(self):
        self.both('common.nc', {'x': [0.0]})
        write_nc(self.base / 'RESTART_48hrs/MOM.res_1.nc', {'x': [1.0]})
        report = self.compare()
        self.assertEqual(report.removed_restart, ['RESTART_48hrs/MOM.res_1.nc'])
        self.assertTrue(report.answer_change)

    def test_removed_restart_file_is_an_answer_change(self):
        self.both('common.nc', {'x': [0.0]})
        write_nc(self.base / 'RESTART_48hrs/ice_cobalt.res.nc', {'x': [1.0]})
        report = self.compare()
        self.assertEqual(report.removed_restart, ['RESTART_48hrs/ice_cobalt.res.nc'])
        self.assertTrue(report.answer_change)


class TestOtherChanges(CompareTestCase):
    def test_added_diag_variable_is_not_an_answer_change(self):
        rel = '20040101.ocean_cobalt.nc'
        write_nc(self.base / rel, {'chl': [1.0]})
        write_nc(self.pr / rel, {'chl': [1.0], 'new_diag': [3.0]})
        report = self.compare()
        self.assertFalse(report.answer_change)
        self.assertEqual(report.added, ['20040101.ocean_cobalt.nc: new_diag'])

    def test_removed_diag_file_is_not_an_answer_change(self):
        self.both('common.nc', {'x': [0.0]})
        write_nc(self.base / '20040101.ocean_extra.nc', {'chl': [1.0]})
        report = self.compare()
        self.assertFalse(report.answer_change)
        self.assertEqual(report.removed, ['20040101.ocean_extra.nc'])

    def test_added_file(self):
        self.both('common.nc', {'x': [0.0]})
        write_nc(self.pr / '20040101.new.nc', {'chl': [1.0]})
        self.assertEqual(self.compare().added, ['20040101.new.nc'])

    def test_parameter_doc_change_is_not_an_answer_change(self):
        write_text(self.base / 'COBALT_parameter_doc.all', 'A = 1\n')
        write_text(self.pr / 'COBALT_parameter_doc.all', 'A = 1\nNEW_OPTION = False\n')
        write_text(self.base / 'available_diags.000000', 'chl\n')
        write_text(self.pr / 'available_diags.000000', 'chl\n')
        report = self.compare()
        self.assertFalse(report.answer_change)
        [diff] = report.param_diffs
        self.assertEqual(diff.file, 'COBALT_parameter_doc.all')
        self.assertIn('+NEW_OPTION = False', diff.lines)
        text = compare_1d.headline(report)
        self.assertIn('No answer change', text)
        self.assertIn('parameters / diag list changed in 1 file', text)

    def test_attribute_only_change_is_metadata(self):
        write_nc(self.base / 'a.nc', {'chl': [1.0]}, var_attrs={'chl': {'units': 'mg'}})
        write_nc(self.pr / 'a.nc', {'chl': [1.0]}, var_attrs={'chl': {'units': 'kg'}})
        report = self.compare()
        self.assertFalse(report.answer_change)
        self.assertEqual(report.metadata, ['a.nc: chl: attribute units changed'])

    def test_global_attribute_change_is_metadata(self):
        write_nc(self.base / 'a.nc', {'chl': [1.0]}, global_attrs={'title': 'a'})
        write_nc(self.pr / 'a.nc', {'chl': [1.0]}, global_attrs={'title': 'b'})
        report = self.compare()
        self.assertEqual(report.metadata, ['a.nc: (global): attribute title changed'])

    def test_checksum_attribute_not_repeated_for_changed_data(self):
        attrs_a, attrs_b = {'t': {'checksum': 'A'}}, {'t': {'checksum': 'B'}}
        write_nc(self.base / 'a.res.nc', {'t': [1.0]}, var_attrs=attrs_a)
        write_nc(self.pr / 'a.res.nc', {'t': [2.0]}, var_attrs=attrs_b)
        report = self.compare()
        self.assertEqual(len(report.var_diffs), 1)
        self.assertEqual(report.metadata, [])

    def test_variable_length_strings(self):
        for side, value in ((self.base, 'kg m-3'), (self.pr, 'mg m-3')):
            path = side / DIAG
            with netCDF4.Dataset(path, 'w', format='NETCDF4') as ds:
                ds.createDimension('n', 2)
                var = ds.createVariable('labels', str, ('n',))
                var[0], var[1] = 'chl', value
        [diff] = self.compare().diag_diffs
        self.assertEqual((diff.name, diff.n_diff), ('labels', 1))

    def test_distributed_file_is_read_as_netcdf(self):
        write_nc(self.base / '20040102.ocean_daily_subset.nc.0000', {'chl': [1.0]})
        write_nc(self.pr / '20040102.ocean_daily_subset.nc.0000', {'chl': [2.0]})
        [diff] = self.compare().diag_diffs
        self.assertEqual(diff.reason, 'data')


class TestReportTable(CompareTestCase):
    def test_one_row_per_variable_across_restart_folders(self):
        for folder in ('RESTART_24hrs', 'RESTART_48hrs'):
            write_nc(self.base / folder / 'MOM.res.nc', {'dic': [1.0, 2.0]})
            write_nc(self.pr / folder / 'MOM.res.nc', {'dic': [1.0, 3.0]})
        rows = compare_1d.var_diff_rows(self.compare().var_diffs, limit=50)
        dic_rows = [row for row in rows if '`dic`' in row]
        self.assertEqual(len(dic_rows), 1)
        self.assertIn('RESTART_24hrs, RESTART_48hrs', dic_rows[0])

    def test_same_name_in_different_files_gets_separate_rows(self):
        # ocean.stats.nc holds a stock total under the same name as the tracer
        rel_stats = 'RESTART_48hrs/ocean.stats.nc'
        write_nc(self.base / STATE, {'chl': [1.0, 2.0]})
        write_nc(self.pr / STATE, {'chl': [1.0, 3.0]})
        write_nc(self.base / rel_stats, {'chl': [500.0]})
        write_nc(self.pr / rel_stats, {'chl': [900.0]})
        report = self.compare()
        rows = compare_1d.var_diff_rows(report.var_diffs, limit=50)
        chl_rows = [row for row in rows if '`chl`' in row]
        self.assertEqual(len(chl_rows), 2)
        self.assertTrue(any('MOM.res.nc' in r and '1.000e+00' in r for r in chl_rows))
        self.assertTrue(
            any('ocean.stats.nc' in r and '4.000e+02' in r for r in chl_rows)
        )
        self.assertIn(
            'restart files: 1 variable differs in 1 file; '
            'ocean.stats.nc: 1 variable differs in 1 file',
            compare_1d.headline(report),
        )

    def test_answer_line_splits_restart_stats_and_text_files(self):
        write_nc(self.base / STATE, {'dic': [1.0], 'co3_ion': [1.0]})
        write_nc(self.pr / STATE, {'dic': [2.0]})
        rel_stats = 'RESTART_48hrs/ocean.stats.nc'
        write_nc(self.base / rel_stats, {'dic': [1.0], 'alk': [1.0]})
        write_nc(self.pr / rel_stats, {'dic': [2.0], 'alk': [2.0]})
        write_text(self.base / 'RESTART_48hrs/ocean.stats', 'En 1.0\n')
        write_text(self.pr / 'RESTART_48hrs/ocean.stats', 'En 1.1\n')
        report = self.compare()
        parts = (
            'restart files: 1 variable differs in 1 file; '
            'ocean.stats.nc: 2 variables differ in 1 file; '
            '1 text file differs; 1 restart item removed'
        )
        self.assertIn(f'**Answer change:** {parts}.', compare_1d.headline(report))
        self.assertIn(
            f'| 1. Answers (model state) | CHANGED: {parts} |',
            compare_1d.render_markdown(report),
        )

    def test_restart_rows_come_first(self):
        rel_stats = 'RESTART_48hrs/ocean.stats.nc'
        write_nc(self.base / rel_stats, {'alk': [1.0]})
        write_nc(self.pr / rel_stats, {'alk': [2.0]})
        write_nc(self.base / STATE, {'zoo': [1.0]})
        write_nc(self.pr / STATE, {'zoo': [2.0]})
        rows = compare_1d.var_diff_rows(self.compare().var_diffs, limit=50)
        self.assertIn('`MOM.res.nc`', rows[2])
        self.assertIn('`ocean.stats.nc`', rows[3])

    def test_outputs_line_counts_restart_items(self):
        write_nc(self.base / STATE, {'dic': [1.0]})
        write_nc(self.pr / STATE, {'dic': [1.0], 'new_tracer': [1.0]})
        write_nc(self.pr / '20040101.new.nc', {'chl': [1.0]})
        write_nc(self.base / 'RESTART_48hrs/ice_cobalt.res.nc', {'x': [1.0]})
        report = self.compare()
        text = compare_1d.render_markdown(report)
        self.assertIn(
            '| 3. Outputs added or removed | '
            '2 added (1 in restart files), 1 removed (1 in restart files) |',
            text,
        )
        self.assertIn(
            'Also: 2 outputs added (1 in restart files), '
            '1 output removed (1 in restart files).',
            compare_1d.headline(report),
        )

    def test_added_restart_variable_is_not_an_answer_change(self):
        write_nc(self.base / STATE, {'dic': [1.0]})
        write_nc(self.pr / STATE, {'dic': [1.0], 'new_tracer': [1.0]})
        report = self.compare()
        self.assertFalse(report.answer_change)
        text = compare_1d.headline(report)
        self.assertIn('1 output added (1 in restart files)', text)

    def test_both_runs_missing_reads_well(self):
        for side in (self.base, self.pr):
            (side / RESTART_MARKER).unlink()
        text = compare_1d.headline(self.compare())
        self.assertIn('base and PR runs', text)

    def test_headline_counts_variables_not_variable_file_pairs(self):
        for folder in ('RESTART_24hrs', 'RESTART_48hrs'):
            write_nc(self.base / folder / 'MOM.res.nc', {'dic': [1.0], 'alk': [1.0]})
            write_nc(self.pr / folder / 'MOM.res.nc', {'dic': [2.0], 'alk': [2.0]})
        text = compare_1d.headline(self.compare())
        self.assertIn('restart files: 2 variables differ in 2 files', text)

    def test_log_has_every_variable_summary_is_shortened(self):
        names = [f'v{i:03d}' for i in range(60)]
        write_nc(self.base / STATE, {n: [1.0] for n in names})
        write_nc(self.pr / STATE, {n: [2.0] for n in names})
        report = self.compare()
        self.assertNotIn('more', compare_1d.render_markdown(report, max_rows=None))
        self.assertIn(
            '... and 10 more', compare_1d.render_markdown(report, max_rows=50)
        )

    def test_long_text_diff_is_shortened_in_the_summary_only(self):
        new_lines = ''.join(f'P{i:02d} = 1\n' for i in range(60))
        write_text(self.base / 'MOM_parameter_doc.all', 'A = 1\n')
        write_text(self.pr / 'MOM_parameter_doc.all', 'A = 1\n' + new_lines)
        report = self.compare()
        log = compare_1d.render_markdown(report, max_rows=None)
        summary = compare_1d.render_markdown(report, max_rows=50)
        self.assertIn('+P59 = 1', log)
        self.assertNotIn('+P59 = 1', summary)
        self.assertIn('... and 23 more', summary)


class TestDiagnostics(CompareTestCase):
    """Diagnostics changes are reported but are not an answer change."""

    def test_diag_only_change_is_not_an_answer_change(self):
        write_nc(self.base / DIAG, {'chl': [1.0, 2.0]})
        write_nc(self.pr / DIAG, {'chl': [1.0, 2.5]})
        report = self.compare()
        self.assertFalse(report.answer_change)
        self.assertTrue(report.diagnostics_change)
        [diff] = report.diag_diffs
        self.assertEqual((diff.name, diff.n_diff), ('chl', 1))
        self.assertEqual(report.var_diffs, [])
        text = compare_1d.headline(report)
        self.assertIn('No answer change', text)
        self.assertIn('diagnostics', text)

    def test_ocean_stats_nc_is_model_state(self):
        write_nc(self.base / 'ocean.stats.nc', {'dic': [1.0]})
        write_nc(self.pr / 'ocean.stats.nc', {'dic': [2.0]})
        report = self.compare()
        self.assertTrue(report.answer_change)
        self.assertFalse(report.diagnostics_change)

    def test_state_and_diag_changes_are_both_reported(self):
        write_nc(self.base / STATE, {'dic': [1.0]})
        write_nc(self.pr / STATE, {'dic': [2.0]})
        write_nc(self.base / DIAG, {'chl': [1.0]})
        write_nc(self.pr / DIAG, {'chl': [2.0]})
        report = self.compare()
        self.assertTrue(report.answer_change)
        self.assertTrue(report.diagnostics_change)
        self.assertEqual([d.name for d in report.var_diffs], ['dic'])
        self.assertEqual([d.name for d in report.diag_diffs], ['chl'])

    def test_report_says_only_daily_diags_hold_data(self):
        self.both(DIAG, {'chl': [1.0]})
        text = compare_1d.render_markdown(self.compare())
        self.assertIn('only the daily diag files hold data', text)


class TestEnvironment(CompareTestCase):
    def test_different_setup_is_not_valid(self):
        self.both('common.nc', {'x': [0.0]})
        env = ENV.format(role='pr', sha='bbb').replace('abc123', 'zzz')
        write_text(self.pr / 'env.txt', env)
        report = self.compare()
        self.assertFalse(report.valid)
        self.assertEqual(report.env_mismatch, {'cefi_sha': ['abc123', 'zzz']})
        self.assertIn('not valid', compare_1d.headline(report))

    def test_different_cpu_is_info_only(self):
        base_env = ENV.format(role='base', sha='a') + 'cpu_model=EPYC 7763\n'
        write_text(self.base / 'env.txt', base_env)
        pr_env = ENV.format(role='pr', sha='b') + 'cpu_model=EPYC 9V74\n'
        write_text(self.pr / 'env.txt', pr_env)
        write_nc(self.base / STATE, {'o2': [1.0]})
        write_nc(self.pr / STATE, {'o2': [1.5]})
        report = self.compare()
        self.assertTrue(report.valid)
        self.assertIn('cpu_model', report.env_info)
        self.assertNotIn('possibly', compare_1d.headline(report))

    def test_env_key_on_one_side_only_is_not_valid(self):
        self.both('common.nc', {'x': [0.0]})
        pr_env = ENV.format(role='pr', sha='bbb') + 'mpi=Open MPI 4.1\n'
        write_text(self.pr / 'env.txt', pr_env)
        report = self.compare()
        self.assertFalse(report.valid)
        self.assertEqual(report.env_mismatch, {'mpi': ['(missing)', 'Open MPI 4.1']})

    def test_unreadable_file_makes_comparison_incomplete(self):
        for side in (self.base, self.pr):
            (side / 'broken.nc').write_bytes(b'CDF\x01' + b'not a netCDF file' * 4)
        report = self.compare()
        self.assertEqual(len(report.errors), 1)
        self.assertFalse(report.valid)
        self.assertIn('Comparison incomplete', compare_1d.headline(report))
        _, out = self.run_main_on_report(report)
        self.assertIn('::warning', out)

    def test_missing_env_is_reported(self):
        self.both('common.nc', {'x': [0.0]})
        (self.base / 'env.txt').unlink()
        report = self.compare()
        self.assertEqual(report.env_missing, ['base'])
        self.assertTrue(report.valid)

    def test_folder_with_only_env_counts_as_missing(self):
        (self.pr / RESTART_MARKER).unlink()
        report = self.compare()
        self.assertEqual(report.missing, ['PR'])
        self.assertFalse(report.answer_change)

    def test_failed_build_with_tracked_parameter_docs_is_missing(self):
        # CEFI keeps *_parameter_doc.* in git, so a failed build still has them.
        (self.base / RESTART_MARKER).unlink()
        for side in (self.base, self.pr):
            write_text(side / 'MOM_parameter_doc.all', 'A = 1\n')
        report = self.compare()
        self.assertEqual(report.missing, ['base'])
        self.assertFalse(report.valid)
        self.assertIn('No comparison', compare_1d.headline(report))
        _, out = self.run_main_on_report(report)
        self.assertIn('::warning', out)

    def test_missing_output_folder(self):
        missing = self.pr.parent / 'not_there'
        report = compare_1d.compare_dirs(self.base, missing)
        self.assertEqual(report.missing, ['PR'])
        self.assertIn('No comparison', compare_1d.headline(report))


class TestCommandLine(CompareTestCase):
    def run_main(self, *args):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            status = compare_1d.main([str(self.base), str(self.pr), *args])
        return status, out.getvalue()

    def make_answer_change(self):
        write_nc(self.base / 'RESTART_48hrs/MOM.res.nc', {'dic': [1.0]})
        write_nc(self.pr / 'RESTART_48hrs/MOM.res.nc', {'dic': [2.0]})

    def test_report_only_exit_status_is_zero(self):
        self.make_answer_change()
        status, out = self.run_main()
        self.assertEqual(status, 0)
        row = (
            '| 1. Answers (model state) | '
            'CHANGED: restart files: 1 variable differs in 1 file |'
        )
        self.assertIn(row, out)

    def test_exit_status_is_zero_with_missing_output(self):
        args = [str(self.base), str(self.pr / 'none')]
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(compare_1d.main(args), 0)

    def test_summary_and_annotation(self):
        self.make_answer_change()
        summary = self.base.parent / 'summary.md'
        status, out = self.run_main('--summary', str(summary), '--github-annotations')
        self.assertEqual(status, 0)
        text = summary.read_text(encoding='utf-8')
        rows = (
            '1. Answers (model state)',
            '2. Diagnostics',
            '3. Outputs added or removed',
            '4. Parameters / diag list',
            '5. Metadata only',
            'Environment',
        )
        for row in rows:
            self.assertIn(row, text)
        self.assertIn('::warning title=1D answer report::Answer change', out)
        self.assertIn('| 1. Answers (model state) |', out)  # also printed to the log

    def test_diag_only_change_gives_notice(self):
        write_nc(self.base / DIAG, {'chl': [1.0]})
        write_nc(self.pr / DIAG, {'chl': [2.0]})
        status, out = self.run_main('--github-annotations')
        self.assertEqual(status, 0)
        self.assertIn('::notice title=1D answer report::', out)
        self.assertNotIn('::warning', out)

    def test_no_annotation_without_change(self):
        self.both('a.nc', {'dic': [1.0]})
        _, out = self.run_main('--github-annotations')
        self.assertNotIn('::warning', out)


class TestCollectOutputs(unittest.TestCase):
    """collect_1d_outputs.sh decides what gets compared, so test what it copies."""

    collect = SCRIPT.parent / 'collect_1d_outputs.sh'

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.run_dir = self.root / 'CEFI' / 'exps' / 'OM4.single_column.COBALT'
        outputs = [
            'RESTART_48hrs/MOM.res.nc',
            'RESTART_48hrs/ocean.stats',
            'RESTART_24hrs/ocean.stats',
            '20040101.ocean_cobalt_budget.nc',
            '20040102.ocean_daily_subset.nc.0000',
            'MOM_parameter_doc.all',
            'COBALT_parameter_doc.short',
            'available_diags.000000',
            'SIS.available_diags',
            'seaice.stats',
            'stocks.out',
            'sea_ice_geometry.nc',
            'Vertical_coordinate.nc',
            'diag_manifest.yaml.0',
        ]
        not_outputs = [
            'o2_col_budget.nc',
            'ncap2_tmp.nc',
            'out1',
            'err1',
            'org.txt',
            'input.nml',
            'logfile.000000.out',
        ]
        for rel in outputs + not_outputs:
            write_text(self.run_dir / rel, rel)
        self.expected = {*outputs, 'env.txt'}

    def run_collect(self, dest):
        return subprocess.run(
            ['bash', str(self.collect), str(dest), 'pr', 'image:tag'],
            cwd=self.run_dir,
            capture_output=True,
            text=True,
            check=False,
        )

    def test_copies_outputs_only(self):
        dest = self.root / 'out'
        proc = self.run_collect(dest)
        self.assertEqual(proc.returncode, 0, proc.stderr)
        copied = set(compare_1d.list_files(dest))
        self.assertEqual(copied, self.expected)

    def test_env_has_every_key(self):
        dest = self.root / 'out'
        self.run_collect(dest)
        env = compare_1d.read_env(dest / 'env.txt')
        keys = {
            'role',
            'image',
            'ocean_bgc_sha',
            'cefi_sha',
            'gfortran',
            'netcdf_c',
            'netcdf_fortran',
            'mpi',
            'glibc',
            'cpu_model',
        }
        self.assertTrue(keys <= set(env), keys - set(env))
        self.assertEqual((env['role'], env['image']), ('pr', 'image:tag'))

    def test_env_records_submodule_pins_except_ocean_bgc(self):
        cefi = self.root / 'CEFI'
        pins = {'src/MOM6': '1' * 40, 'src/ocean_BGC': '2' * 40}
        write_text(
            cefi / '.gitmodules',
            ''.join(
                f'[submodule "{path}"]\n\tpath = {path}\n\turl = https://x.invalid\n'
                for path in pins
            ),
        )
        git = ['git', '-C', str(cefi), '-c', 'user.name=t', '-c', 'user.email=t@t']
        subprocess.run([*git, 'init', '-q'], check=True)
        for path, sha in pins.items():
            cacheinfo = f'160000,{sha},{path}'
            add = ['update-index', '--add', '--cacheinfo', cacheinfo]
            subprocess.run([*git, *add], check=True)
        subprocess.run([*git, 'add', '.gitmodules'], check=True)
        subprocess.run([*git, 'commit', '-q', '-m', 'pins'], check=True)
        dest = self.root / 'out'
        self.run_collect(dest)
        env = compare_1d.read_env(dest / 'env.txt')
        self.assertEqual(env.get('submodule.src/MOM6'), '1' * 40)
        self.assertNotIn('submodule.src/ocean_BGC', env)

    def test_existing_destination_is_refused(self):
        dest = self.root / 'out'
        dest.mkdir()
        self.assertEqual(self.run_collect(dest).returncode, 2)


if __name__ == '__main__':
    unittest.main()
