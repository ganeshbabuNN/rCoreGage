# rCoreGage 1.1.0

## Bug fixes

- **`count_valid()`: multiple `unblind_codes` only checked first code.**
  `grepl(unblind_codes, desc)` silently used only the first element when
  a vector was passed. Fixed with a nested `vapply` loop that evaluates
  every code independently.

- **`collect_findings()`: crashed on 0-row data frame.**
  Assigning a scalar to a 0-row data frame column with `$<-` caused
  `replacement has 1 row, data has 0`. Fixed with an `nrow > 0` guard;
  0-row input now produces a valid `summary_log` entry with `nu = 0`.

- **`build_reports()`: `all_open.xlsx` could not be read back.**
  `readxl::read_xlsx(range = "A3:L500000")` threw an error on Windows
  when the range exceeded the actual column count (10 columns). Replaced
  with `skip = 2` which auto-detects the column count.

- **`build_reports()`: date parsing failed for ISO string dates.**
  `FIND DATE` was stored as an ISO string (`"2024-01-15"`) by `openxlsx`
  but converted with `as.numeric()` only, producing `NA` for every row.
  Added `.parse_date_col()` helper that tries Excel serial number, ISO
  string, and `ddMMMYYYY` format in sequence.

- **`build_reports()`: `[Was closed but re-appeared]` applied to all rows.**
  The auto-note condition checked the already-reassigned status rather than
  the original status from the previous run. Fixed by preserving
  `old_status` before reassignment and checking it in the auto-note logic.

- **`build_reports()`: `NA` visit IDs failed to match in `merge()`.**
  `merge()` in R does not match `NA == NA`. Fixed using a `-1` sentinel
  value before every merge, restored to `NA` after.

- **`build_reports()`: duplicate auto-tags accumulated across runs.**
  Re-running multiple times appended the same tag with different dates.
  Fixed by stripping existing tags of the same type before appending the
  updated one.

- **`run_checks()`: check scripts could not find `collect_findings()`.**
  `new.env(parent = baseenv())` cut off the `rCoreGage` namespace from
  the check script's lookup chain. Fixed to `new.env(parent = environment())`
  so all exported package functions are visible inside check scripts.

- **`run_checks()`: removed `<<-` global environment modification.**
  The previous `local({}) + <<-` pattern was replaced with
  `new.env() + sys.source()` to comply with CRAN policy prohibiting
  modification of the global environment.

---

## CRAN compliance

- All `\dontrun{}` examples replaced with `\donttest{}`.
- All `@examples` blocks are now fully self-contained using `tempdir()`
  and `system.file()`  no project setup required to run them.
- `stringr` removed from `Imports` field (was declared but never called).
- `utils::read.csv()` qualified; `importFrom(utils, read.csv)` added to
  `NAMESPACE`.
- `utils::globalVariables(".data")` added in `R/globals.R` to suppress
  `R CMD check` NOTE for `dplyr` column references.
- `setNames()` calls replaced with `structure()` and `names() <-` to
  avoid undeclared dependency on the `stats` package.
- Bare `dplyr` column references replaced with `.data$` pronoun in
  `run_checks()`.
- `DESCRIPTION`: software name `'Excel'` quoted; all acronyms (`DM`,
  `MW`, `SDTM`, `ADaM`, `CDISC`) expanded; CDISC reference URL added;
  `Description` field restructured to end with a complete sentence.
- Roxygen `@description` in `runner.R`: `check_<rule_set>` angle brackets
  replaced with `check_RULESET` to prevent LaTeX PDF build errors.
- `vignettes/getting-started.Rmd`: hardcoded `C:/Projects` path replaced
  with `tempdir()`.

---

# rCoreGage 1.0.0

- Initial CRAN release.
- Configuration-driven data quality check framework for clinical and
  analytical data.
- Trial-level and study-level check execution from `rules/trial/` and
  `rules/study/` folders.
- `rule_registry.xlsx` as the central check registry with `Active`,
  report-type, and `Rule_Set` columns.
- `create_project()` scaffolds a complete project folder structure.
- Excel report output via `openxlsx`.
- Feedback loop via `outputs/feedback/` folder.
