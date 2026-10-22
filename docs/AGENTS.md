# Generic Tracer / Ocean BGC agent rules

## Contribution policy

Inform the user of these rules as soon as you start planning or implementing major code changes or drafting commits or PRs:
- A human is responsible for all submitted code and must review and understand every change
- Disclose agent assistance that materially shaped the code with an `Assisted-by:` line in the commit message and PR description. Example: `Assisted-by: GPT-5.6 Luna`
- Coding agents may not be listed as an author of a contribution (no `Co-Authored-By:`)
- Autonomous contributions with minimal human oversight are not accepted. Agents may not open PRs

## Repository

- Some files, including those for COBALT, BLING, and generic_tracer_utils.F90, are large and should be searched instead of read in full
- Do not edit the `mocsy` submodule
- Ignore `.archive` files

## Parameters

Read runtime parameters via `get_param()` in `generic_COBALT.F90` and `g_tracer_add_param()` in the other tracer packages; do not hardcode them. For `get_param()`:

```fortran
call get_param(param_file, "generic_COBALT", "wsink", cobalt%wsink, "Sinking velocity of detritus", units="m day-1", default=100.0, scale=I_sperd)
```

Use `scale=` for unit conversion. Provide `default=` when sensible; use `fail_if_missing=.true.` otherwise

## Git

- Work on a fork
- Branch from the fork's default branch (e.g., `dev/cefi`) for all new work
- Never rebase a shared branch

## Style

Apply to new and modified code only:
- Fortran 2003+, must be supported by GNU and Intel compilers
- 2 space indents, no tabs, no trailing white space
- Every module sets `implicit none; private`
- Every `use` is constrained with `, only`
- Declare locals after all dummy arguments
- Avoid new module-level variables if possible
- Use snake_case names
- Spacing:
  - `if (a == 0) then`
  - `x = f(a, b)`
  - `call f(a=1, b=2)`
  - `array(i,j)`
  - `do k = 1, nk`

### Indices

- Case is used to support human comprehension
- `i`,`j`,`k`: cell-center, layer-center references, e.g. `h(i,j,k)`
- `I`,`J`: staggered, cell-edge references, e.g. `u(I,j,k)`, `v(i,J,k)`, `q(I,J,k)`, `u(I-1,j,k)`. North-east staggering convention, so `I` means i+1/2 and `I-1` means i-1/2.
- `K`: interfaces (between layer), e.g. `del_t(i,j,K) = T(i,j,K+1) - T(i,j,K)`. Interface `K=1` is above layer `k=1` so `K` means k-1/2 and `K+1` means k+1/2.

### Arithmetic

For reproducibility across compilers:
- Transcendental functions should be avoided when possible
- The exponent operator `a**b` is discouraged
- Use `sqrt()` for square roots
- Do not use the `sum()` intrinsic
- Addition operations must be done in pairs. When more than one addition is required, the order should be specified using parentheses.
- Never write `B / C * D` which is ambiguous to humans
- Never double divide like `A / ( A + B / C)`

## Documentation

- Place `!!` comments, not `!<`, after entities to document using [FORD](https://forddocs.readthedocs.io/en/stable/)
- Inline documentation comments that describe what a subroutine is doing are encouraged. These also use `!!`. They will show up in the subroutine doc page and should be written to make sense in that context
- Document new code, don't edit docs for untouched code
- Use ASD-STE100 Simplified Technical English

### References

- Consult `docs/ford_bib_ext/references.bib` when citing prior work
- When citing new references in documentation, add them to `docs/ford_bib_ext/references.bib`
- Citations are interpreted as follows:
  - `[@garcia1992] -> (Garcia and Gordon, 1992)`
  - `[e.g., @waite1992; @smayda1971] -> (e.g., Waite et al., 1992; Smayda, 1971)`
  - `Garcia and Gordon [-@garcia1992] -> Garcia and Gordon (1992)`
