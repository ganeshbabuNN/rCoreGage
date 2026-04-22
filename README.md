# rCoreGage 

![version](https://img.shields.io/badge/version-0.1.0-blue)
![license](https://img.shields.io/badge/license-GPL--3.0-green)

> **Data Quality Check Framework for Clinical and Analytical Data**

rCoreGage is a configuration-driven R package for running domain-level data
quality checks and consolidating findings into structured Excel reports with
role-based feedback routing. It is designed to be installed once from CRAN and
reused across any number of trials or studies without changing the engine code.

***

## Table of Contents
  
1. [Why rCoreGage](#1-why-rcoregage)
2. [Architecture — Two Layers](#2-architecture--two-layers)
3. [Installation](#3-installation)
4. [Quick Start](#4-quick-start)
5. [Project Structure](#5-project-structure)
6. [rule_registry.xlsx — Check Definitions](#6-rule_registryxlsx--check-definitions)
7. [How It Works — Layered Execution Model](#7-how-it-works--layered-execution-model)
   - 7.1 [Phase 1 — Setup](#71-phase-1--setup)
   - 7.2 [Phase 2 — Load Inputs](#72-phase-2--load-inputs)
   - 7.3 [Phase 3 — Trial Level Checks](#73-phase-3--trial-level-checks)
   - 7.4 [Phase 4 — Study Level Checks](#74-phase-4--study-level-checks)
   - 7.5 [Phase 5 — Report Generation](#75-phase-5--report-generation)
8. [Trial vs Study Level — Full Comparison](#8-trial-vs-study-level--full-comparison)
9. [The Feedback Loop](#9-the-feedback-loop)
10. [Writing Check Scripts](#10-writing-check-scripts)
11. [Engine Functions Reference](#11-engine-functions-reference)
12. [Console Output Reference](#12-console-output-reference)
13. [Features](#13-features)
14. [Examples](#14-examples)
15. [Troubleshooting](#15-troubleshooting)
16. [Contributing](#16-contributing)

***

## 1. Why rCoreGage

Clinical data quality checking typically involves:

- Running the same checks across dozens of domains (AE, LB, CM, VS ...)
- Separating trial-specific rules from study-wide rules
- Distributing findings to different roles (Data Management, Medical Writing,
  SDTM, ADaM programmers) and tracking their responses
- Carrying reviewer notes forward across repeated runs without losing history 

Most existing approaches embed all of this logic in monolithic scripts that are
copy-pasted per trial, with no separation between the engine and the rules.

`rCoreGage` solves this by:

| Problem | rCoreGage solution |
|---|---|
| Engine scattered across trials | Engine installed once via `install.packages()` |
| Hard-coded paths | Single `project_config.R` per project |
| No role separation in reports | Four separate report channels (DM / MW / SDTM / ADAM) |
| Feedback lost between runs | Structured feedback folders merged on every re-run |
| SAS-origin terminology | Fully generic — works for any tabular data domain |

***

## 2. Architecture — Two Layers

```
┌─────────────────────────────────────────────────────────────────┐
│  LAYER 1 — rCoreGage PACKAGE  (install once from CRAN)          │
│                                                                 │
│  R/                                                             │
│    setup.R      setup_coregage()   reads rule_registry          │
│    runner.R     run_checks()       loops and executes scripts   │
│    reporter.R   build_reports()    merges feedback + writes xlsx │
│    collector.R  collect_findings() appends findings to state    │
│    counter.R    count_valid()      counts observations          │
│    project.R    create_project()   scaffolds new project        │
│    utils.R      load_inputs()      reads domain data files      │
│                                                                 │
│  inst/                                                          │
│    extdata/rule_registry.xlsx    blank check registry template  │
│    templates/run_coregage.R      driver script template         │
│    templates/project_config.R   path config template           │
│    templates/Check_Template.R   blank check scaffold           │
└─────────────────────────────────────────────────────────────────┘
                          │
              create_project("TRIAL_ABC")
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│  LAYER 2 — USER PROJECT  (one folder per trial/study)           │
│                                                                 │
│  TRIAL_ABC/                                                     │
│    TRIAL_ABC.Rproj                                              │
│    run_coregage.R           driver — source() to run            │
│    rules/                                                       │
│      config/                                                    │
│        rule_registry.xlsx   check definitions (user fills in)   │
│        project_config.R     all paths in one place              │
│      trial/                 trial-level check scripts           │
│        AE.R  LB.R  CM.R ... check_AE(state, cfg)               │
│      study/                 study-level check scripts           │
│        AE_study.R  LB_study.R ... check_AE_study(state, cfg)   │
│    inputs/                  drop domain CSV / sas7bdat here     │
│    outputs/                                                     │
│      reports/               Excel reports written here          │
│      feedback/              reviewer feedback placed here       │
│        DM/  MW/  SDTM/  ADAM/                                   │
└─────────────────────────────────────────────────────────────────┘
```

**The key separation:** the engine (Layer 1) never changes between trials.
Only the check scripts, `rule_registry.xlsx`, and `inputs/` change per trial.

***
## 3. Installation

### From CRAN (stable release)

```r
install.packages("rCoreGage")
```
### From GitHub (Stable version)

```r
#install.packages("remotes")
remotes::install_github("ganeshbabunn/rCoreGage")
```
### Development version:
```r
#install.packages("remotes")
remotes::install_github("ganeshbabunn/rCoreGage@dev")
```
### From local source
```r
install.packages("../rCoreGage_X.X.tar.gz", repos = NULL, type = "source")
X indicates the version of the release
```

### Dependencies

Installed automatically with the package:

| Package | Version | Purpose |
|---|---|---|
| `readxl` | >= 1.4.0 | Read rule_registry.xlsx and feedback files |
| `openxlsx` | >= 4.2.5 | Write formatted Excel reports |
| `dplyr` | >= 1.1.0 | Data manipulation in check scripts |
| `stringr` | >= 1.5.0 | String helpers |

Optional (for reading SAS transport files):

```r
install.packages("haven")   # enables reading .sas7bdat from inputs/
```

### System requirements

- R >= 4.1.0
- RStudio (recommended, not required)
- Windows, macOS, or Linux

***

## 4. Quick Start

```r
# Step 1 — Install (once)
install.packages("rCoreGage")

# Step 2 — Create a new project (once per trial)
rCoreGage::create_project(
  name = "TRIAL_ABC",
  path = "C:/Projects"
)

# Step 3 — Open TRIAL_ABC.Rproj in RStudio

# Step 4 — Fill in rules/config/rule_registry.xlsx
#           (Trial sheet + Study sheet — see Section 6)

# Step 5 — Write check scripts in rules/trial/ and rules/study/
#           (copy Check_Template.R and implement your logic)

# Step 6 — Drop domain data files into inputs/
#           AE.csv, LB.csv, CM.csv ... (CSV or .sas7bdat)

# Step 7 — Run
source("run_coregage.R")

# Reports appear in outputs/reports/
```

**Expected console output:**

```
========== CoreGage : Starting Run ==========
  Project : TRIAL_ABC
>> [setup] Starting CoreGage initialisation ...
  Sheet 'Trial' rows : 33
  Sheet 'Study' rows : 33
  Active: 66 ON  /  0 OFF
>> Loading domain inputs ...
   AE.csv -> domains$ae  (81 rows)
   LB.csv -> domains$lb  (1003 rows)
>>>>>>>>>>>>>>>>>>>>>> Executing: AE <<<<<<<<<<<<<<<<<<<<
  Running AECHK001 - AE end date before start date ...
  >> [collector] Appending 5 finding(s) for: AECHK001
...
>> [reporter] Starting consolidation ...
  -------------------------------------------------------
  Feedback summary:
    Notes  : analyst notes: 0  |  reviewer notes: 0
    Status : open: 147  |  queried: 0  |  closed: 0
  -------------------------------------------------------
  Writing: DM_issues.xlsx
  Writing: all_open.xlsx
========== CoreGage : Run Complete ==========
>> Reports: C:/Projects/TRIAL_ABC/outputs/reports
```

---

## 5. Project Structure

After `create_project()`, your project folder contains:

```
TRIAL_ABC/
│
├── TRIAL_ABC.Rproj          Open this in RStudio
├── run_coregage.R           Source this to run everything
├── .gitignore               inputs/ and outputs/ excluded from git
│
├── rules/
│   ├── config/
│   │   ├── rule_registry.xlsx    Check definitions (fill this in)
│   │   └── project_config.R      All folder paths in one file
│   │
│   ├── trial/               TRIAL-LEVEL check scripts
│   │   ├── AE.R             check_AE(state, cfg)
│   │   ├── LB.R             check_LB(state, cfg)
│   │   ├── CM.R             check_CM(state, cfg)
│   │   └── ...
│   │
│   └── study/               STUDY-LEVEL check scripts
│       ├── AE_study.R       check_AE_study(state, cfg)
│       ├── LB_study.R       check_LB_study(state, cfg)
│       └── ...
│
├── inputs/                  Drop domain data files here
│   ├── AE.csv               → automatically loaded as domains$ae
│   ├── LB.csv               → automatically loaded as domains$lb
│   └── *.sas7bdat           → supported if haven is installed
│
└── outputs/
    ├── reports/             Excel reports written here on every run
    │   ├── DM_issues.xlsx
    │   ├── MW_issues.xlsx
    │   ├── SDTM_issues.xlsx
    │   ├── ADAM_issues.xlsx
    │   ├── all_open.xlsx
    │   └── all_closed.xlsx
    │
    └── feedback/            Reviewers place updated files here
        ├── DM/
        ├── MW/
        ├── SDTM/
        └── ADAM/
```

### project_config.R

All paths for a project are defined in one file. Edit this if you move the
project to a different machine:

```r
# rules/config/project_config.R
project_root <- getwd()   # resolves correctly when .Rproj is open

cfg <- list(
  project_name  = "TRIAL_ABC",
  rule_registry = file.path(project_root, "rules/config/rule_registry.xlsx"),
  trial_checks  = file.path(project_root, "rules/trial"),
  study_checks  = file.path(project_root, "rules/study"),
  inputs        = file.path(project_root, "inputs"),
  reports       = file.path(project_root, "outputs/reports"),
  feedback      = file.path(project_root, "outputs/feedback")
)
```

***

## 6. rule_registry.xlsx — Check Definitions

The rule registry is the central configuration file. It has two sheets:
**Trial** and **Study**. Both sheets have identical columns.

### Column definitions

| Column | Type | Purpose |
|---|---|---|
| `Category` | Text | Domain or functional grouping e.g. `Adverse Events` |
| `Subcategory` | Text | Check type e.g. `Date Checks`, `Completeness` |
| `ID` | Text | **Unique** check identifier e.g. `AECHK001` |
| `Active` | `Yes` / `No` | `Yes` = run this check. `No` = skip silently |
| `DM_Report` | `Yes` / `No` | Include findings in `DM_issues.xlsx` |
| `MW_Report` | `Yes` / `No` | Include findings in `MW_issues.xlsx` |
| `SDTM_Report` | `Yes` / `No` | Include findings in `SDTM_issues.xlsx` |
| `ADAM_Report` | `Yes` / `No` | Include findings in `ADAM_issues.xlsx` |
| `Rule_Set` | Text | Name of the `.R` file containing this check's logic |
| `Description` | Text | Plain English description shown in report Summary tab |
| `Notes` | Text | Free-text implementation notes |

### Example rows (Trial sheet)

| Category | Subcategory | ID | Active | DM_Report | MW_Report | SDTM_Report | ADAM_Report | Rule_Set | Description |
|---|---|---|---|---|---|---|---|---|---|
| Adverse Events | Date Checks | AECHK001 | Yes | Yes | No | Yes | No | AE | AE end date before start date |
| Adverse Events | Completeness | AECHK002 | Yes | No | No | Yes | No | AE | Missing AE severity (AESEV) |
| Laboratory | Range Checks | LBCHK001 | Yes | Yes | No | Yes | Yes | LB | Lab value outside normal range |

### Example rows (Study sheet)

| Category | Subcategory | ID | Active | DM_Report | MW_Report | SDTM_Report | ADAM_Report | Rule_Set | Description |
|---|---|---|---|---|---|---|---|---|---|
| Adverse Events | Serious AE | AEPRJ001 | Yes | Yes | Yes | No | No | AE_study | Serious AE missing AEACN |
| Laboratory | Completeness | LBPRJ001 | Yes | Yes | No | Yes | Yes | LB_study | Missing lab collection date |

### How `Rule_Set` maps to a file

The `Rule_Set` value must exactly match the filename (without `.R`) of the
check script:

```
Rule_Set = "AE"        →  rules/trial/AE.R        (function: check_AE)
Rule_Set = "AE_study"  →  rules/study/AE_study.R  (function: check_AE_study)
Rule_Set = "LB"        →  rules/trial/LB.R        (function: check_LB)
```

***

## 7. How It Works — Layered Execution Model

Every run of `source("run_coregage.R")` passes through five sequential phases.

```
source("run_coregage.R")
         │
         ▼
┌─────────────────────────┐
│  Phase 1 — Setup        │  setup_coregage(cfg)
│  Read rule_registry     │  → builds state object
│  Build active_rules     │  → state$rule_registry
│  Init empty tables      │  → state$active_rules
└────────────┬────────────┘  → state$issues = []
             │
             ▼
┌─────────────────────────┐
│  Phase 2 — Load Inputs  │  load_inputs(cfg)
│  Scan inputs/ folder    │  → state$domains$ae
│  Read all CSV / sas7bdat│  → state$domains$lb
│  Named by filename      │  → state$domains$cm  ...
└────────────┬────────────┘
             │
             ▼
┌─────────────────────────┐
│  Phase 3 — Trial Checks │  run_checks(cfg, state)
│  Loop Trial sheet       │  → sources rules/trial/AE.R
│  Call check_AE()        │  → calls check_AE(state, cfg)
│  Call check_LB()  ...   │  → collect_findings() appends to state$issues
└────────────┬────────────┘
             │
             ▼
┌─────────────────────────┐
│  Phase 4 — Study Checks │  run_checks() (continued)
│  Loop Study sheet       │  → sources rules/study/AE_study.R
│  Call check_AE_study()  │  → calls check_AE_study(state, cfg)
│  Call check_LB_study()  │  → collect_findings() appends to state$issues
└────────────┬────────────┘
             │
             ▼
┌─────────────────────────┐
│  Phase 5 — Reports      │  build_reports(cfg, state)
│  Import previous run    │  → reads all_open.xlsx, all_closed.xlsx
│  Merge old + new        │  → compares by CHECK ID + SUBJECT + DESCRIPTION
│  Read feedback folders  │  → reads feedback/DM/, /MW/, /SDTM/, /ADAM/
│  Merge reviewer notes   │  → analyst_note, review_note, status updated
│  Write 6 Excel reports  │  → DM/MW/SDTM/ADAM/all_open/all_closed
└─────────────────────────┘
```

### 7.1 Phase 1 — Setup

`setup_coregage(cfg)` reads `rule_registry.xlsx` and returns a **state object** —
a named list that flows through every subsequent phase carrying all data and
results.

```r
state <- setup_coregage(cfg)

# state contains:
state$rule_registry   # data frame: all check definitions
state$active_rules    # named logical: TRUE = check is ON
state$session         # list: run date and time
state$issues          # data frame: empty — filled by check scripts
state$summary_log     # data frame: empty — filled by collector
state$review_log      # data frame: empty — filled by reporter
```

### 7.2 Phase 2 — Load Inputs

`load_inputs(cfg)` scans `inputs/` and reads every `.csv` and `.sas7bdat` into
a named list. The name is the lowercase filename without extension:

```
inputs/AE.csv     →  state$domains$ae    (case-insensitive)
inputs/LB.CSV     →  state$domains$lb
inputs/CM.sas7bdat →  state$domains$cm   (requires haven)
```

No code change is needed when adding a new domain — just drop the file in
`inputs/` and it is loaded automatically.

### 7.3 Phase 3 — Trial Level Checks

Trial-level checks are **trial-specific rules** defined in the `Trial` sheet of
`rule_registry.xlsx` and implemented in `rules/trial/`.

```
rule_registry.xlsx (Trial sheet)
  Rule_Set = "AE"   Active = "Yes"
       │
       ▼
runner.R reads:  rules/trial/AE.R
       │
       ▼
calls:  check_AE(state, cfg)
       │
       ▼
check_AE filters domains$ae, builds findings data frame
       │
       ▼
collect_findings(state, AECHK001_df, id = "AECHK001")
       │
       ▼
state$issues now has rows for AECHK001
```

**Typical use cases for trial-level checks:**
- Date range checks specific to this trial's protocol
- Missing field checks for required variables
- Inclusion/exclusion criteria violations
- Dose ranges specific to this drug/dosing regimen

### 7.4 Phase 4 — Study Level Checks

Study-level checks are **cross-trial or study-programme-wide rules** defined in
the `Study` sheet and implemented in `rules/study/`.

```
rule_registry.xlsx (Study sheet)
  Rule_Set = "AE_study"   Active = "Yes"
       │
       ▼
runner.R reads:  rules/study/AE_study.R
       │
       ▼
calls:  check_AE_study(state, cfg)
       │
       ▼
check_AE_study filters domains$ae, builds findings
       │
       ▼
collect_findings(state, AEPRJ001_df, id = "AEPRJ001")
       │
       ▼
state$issues now has rows for AEPRJ001 (alongside AECHK001)
```

**Typical use cases for study-level checks:**
- Dictionary coding completeness (AEDECOD, MHDECOD)
- Cross-domain consistency checks
- ADaM derivation prerequisites (study day, baseline flags)
- Checks that apply identically across all trials in a programme

Both levels feed into **the same `state$issues` table** and are treated
identically by `build_reports()` — the only difference is which report
channels they appear in, controlled by the `DM_Report` / `MW_Report` /
`SDTM_Report` / `ADAM_Report` columns.

### 7.5 Phase 5 — Report Generation

`build_reports()` performs five sub-steps:

```
1. Import history
   Reads all_open.xlsx and all_closed.xlsx from the previous run.
   Extracts: CHECK ID, SUBJECT ID, VISIT ID, DESCRIPTION, STATUS,
             ANALYST NOTE, ANALYST ID

2. Merge new + old findings
   Matches by: CHECK ID + SUBJECT ID + VISIT ID + DESCRIPTION (whitespace-stripped)
   Assigns status:
     New finding (not seen before)           → Open
     Still in data, was open                 → carry status forward
     Analyst wrote "closed by analyst" + initials → Closed
     Not in data anymore                     → Closed (auto-tag in note)
     Was Closed, reappeared                  → re-open (if closed by analyst)

3. Read role feedback folders
   Reads all .xlsx files from feedback/DM/, /MW/, /SDTM/, /ADAM/
   Picks most recent file per finding (by file modification time)
   Merges: ANALYST NOTE, ANALYST ID, REVIEW NOTE, REVIEWER ID, STATUS
   Reviewer-closed findings tagged [closed by reviewer] to persist closure

4. Summarise
   Counts open / queried / closed per check
   Calculates analyst note count and reviewer note count

5. Write reports
   DM_issues.xlsx   — checks where DM_Report = Yes, open findings
   MW_issues.xlsx   — checks where MW_Report = Yes, open findings
   SDTM_issues.xlsx — checks where SDTM_Report = Yes, open findings
   ADAM_issues.xlsx — checks where ADAM_Report = Yes, open findings
   all_open.xlsx    — all active checks, all open findings
   all_closed.xlsx  — all checks, all closed findings
```

***

## 8. Trial vs Study Level — Full Comparison

| Dimension | Trial Level | Study Level |
|---|---|---|
| **Sheet in rule_registry.xlsx** | `Trial` | `Study` |
| **Script location** | `rules/trial/` | `rules/study/` |
| **Function naming** | `check_AE(state, cfg)` | `check_AE_study(state, cfg)` |
| **Filename** | `AE.R` | `AE_study.R` |
| **Typical scope** | Protocol-specific rules for this trial | Consistent rules across all trials in a programme |
| **Data source** | Same `state$domains` — no difference | Same `state$domains` — no difference |
| **Findings destination** | Same `state$issues` table | Same `state$issues` table |
| **Report routing** | Controlled by `DM_Report` etc. columns | Controlled by same columns |
| **Change between trials?** | Yes — copy and customise per trial | No — deployed once, shared across trials |

**Layered view for a single domain (AE):**

```
                    AE.csv (inputs/)
                         │
              ┌──────────┴──────────┐
              │                     │
       check_AE()             check_AE_study()
    rules/trial/AE.R         rules/study/AE_study.R
              │                     │
   ┌──────────┤          ┌──────────┤
   │ AECHK001 │ end<start│ AEPRJ001 │ SAE missing AEACN
   │ AECHK002 │ miss AESEV│AEPRJ002 │ missing AEDECOD
   │ AECHK003 │ miss AEOUT│ AEPRJ003 │ missing AESTDY
   └──────────┘          └──────────┘
              │                     │
              └──────────┬──────────┘
                         │
                  state$issues
                         │
                 build_reports()
                         │
          ┌──────────────┼──────────────────┐
          │              │                  │
    DM_issues.xlsx  SDTM_issues.xlsx  all_open.xlsx
    (AECHK001 Yes)  (AECHK001 Yes     (all findings)
    (AEPRJ001 Yes)   AEPRJ002 Yes)
```

***

## 9. The Feedback Loop

The feedback loop allows reviewers to annotate findings and have their
responses automatically carried forward into subsequent reports.

```
┌─────────────────────────────────────────────────────────────────┐
│                      FEEDBACK LOOP                              │
│                                                                 │
│  Run 1: source("run_coregage.R")                                │
│         └─► outputs/reports/DM_issues.xlsx   (all open)        │
│                                                                 │
│  Distribute to Data Manager                                     │
│         DM opens DM_issues.xlsx in Excel                        │
│         DM fills in:                                            │
│           ANALYST NOTE  — their own note                        │
│           ANALYST ID    — their initials                        │
│           STATUS        — Open / Closed / Queried               │
│           REVIEW NOTE   — reviewer comment                      │
│           REVIEWER ID   — reviewer initials                     │
│         DM saves the file to outputs/feedback/DM/               │
│         (any filename is accepted e.g. DM_week1_feedback.xlsx)  │
│                                                                 │
│  Run 2: source("run_coregage.R")                                │
│         └─► Reporter reads feedback/DM/*.xlsx                   │
│         └─► Matches rows by CHECK ID + SUBJECT ID + DESCRIPTION │
│         └─► Merges notes and status back into issuelist         │
│         └─► Writes new DM_issues.xlsx with feedback included    │
│                                                                 │
│  Repeat for MW / SDTM / ADAM independently                      │
└─────────────────────────────────────────────────────────────────┘
```

### Status rules

| Situation | Status assigned |
|---|---|
| Brand new finding | `Open` |
| Finding seen before, status was not Closed | Carry forward as-is |
| Analyst wrote `closed by analyst` + initials present | `Closed` (permanent) |
| Reviewer set STATUS = Closed (with reviewer ID or review note) | `Closed` + `[closed by reviewer]` tag added |
| Finding was Closed, reappeared in data | `Open` + `[Was closed but re-appeared (date).]` |
| Finding disappeared from data | `Closed` + `[Not in data anymore (date).]` |
| Reviewer set STATUS = Queried | `Queried` (carried forward) |

### Console feedback summary

On every run that reads feedback files, the console shows:

```
Importing feedback from: feedback/DM/ (1 file(s))
  -> 1710 finding(s) matched from feedback/DM/
       status updates: 3  |  analyst notes: 1  |  review notes: 1
-------------------------------------------------------
Feedback summary:
  Notes  : analyst notes: 2  |  reviewer notes: 2
  Status : open: 1726  |  queried: 1  |  closed: 3
-------------------------------------------------------
```

***

## 10. Writing Check Scripts

Copy `rules/trial/Check_Template.R` and rename to match your `Rule_Set` value.

### Template structure

```r
# rules/trial/AE.R
# Rule_Set : AE
check_AE <- function(state, cfg) {

  domains      <- state$domains       # named list of domain data frames
  active_rules <- state$active_rules  # named logical: TRUE = check is ON

  # Guard: skip if domain data is missing
  if (is.null(domains$ae) || nrow(domains$ae) == 0) {
    message("  WARNING [AE]: domains$ae is empty - skipping.")
    return(state)
  }
  ae <- domains$ae

  # ── Check: AECHK001 ────────────────────────────────────────────────────────
  if (isTRUE(active_rules["AECHK001"])) {

    AECHK001 <- ae |>
      dplyr::filter(!is.na(AESTDTC), !is.na(AEENDTC)) |>
      dplyr::mutate(
        st = as.Date(AESTDTC),
        en = as.Date(AEENDTC)
      ) |>
      dplyr::filter(!is.na(st), !is.na(en), en < st) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,          # use NA_real_ when no visit applies
        description = paste0(
          "AE end date (", format(en, "%d%b%Y"), ")",
          " before start date (", format(st, "%d%b%Y"), ")",
          " for: ", AETERM, " (AESEQ=", AESEQ, ")"
        )
      ) |>
      dplyr::select(subj_id, vis_id, description)

    state <- collect_findings(state, AECHK001, id = "AECHK001")
  }

  # Add more checks following the same pattern ...

  state   # always return state
}
```

### Rules for check scripts

| Rule | Detail |
|---|---|
| Function name | Must be `check_<Rule_Set>` matching the `Rule_Set` column exactly |
| File name | Must be `<Rule_Set>.R` in `rules/trial/` or `rules/study/` |
| Return | Always return `state` as the last line |
| Result columns | `subj_id` (character), `vis_id` (numeric or `NA_real_`), `description` (character, max 200 chars) |
| Access data | `domains$ae`, `domains$lb` etc. (lowercase domain name) |
| Check guard | `if (isTRUE(active_rules["CHECKID"]))` — never `if (active_rules["CHECKID"])` |
| Domain guard | Always check `is.null(domains$xx) || nrow(domains$xx) == 0` before using |

***

## 11. Engine Functions Reference

### `setup_coregage(cfg)`

Reads `rule_registry.xlsx`, builds the `active_rules` switch vector, and
initializes empty master tables.

```r
state <- setup_coregage(cfg)
```

### `load_inputs(cfg)`

Scans `cfg$inputs` and loads every `.csv` / `.sas7bdat` into `state$domains`.

```r
state$domains <- load_inputs(cfg)
# state$domains$ae, state$domains$lb, etc.
```

### `run_checks(cfg, state)`

Loops through all active rule sets in the Trial and Study sheets, sources the
corresponding `.R` file, and calls `check_<Rule_Set>(state, cfg)`.

```r
state <- run_checks(cfg, state)
```

### `collect_findings(state, df, id, ...)`

Appends a findings data frame to `state$issues`. Called at the end of each
individual check block inside a check script.

```r
state <- collect_findings(state, my_check_df, id = "AECHK001")
```

| Argument | Default | Description |
|---|---|---|
| `state` | required | Current state object |
| `df` | required | Data frame with `subj_id`, `vis_id`, `description` |
| `id` | required | Check ID string matching rule_registry |
| `desc_col` | `"description"` | Column name holding the description text |
| `sobs` | `TRUE` | Apply observation limit |
| `unblind_codes` | `character(0)` | Topic codes that could unblind the study |

### `build_reports(cfg, state)`

Merges findings with history, incorporates role-based feedback, and writes
six Excel reports to `cfg$reports`.

```r
build_reports(cfg, state)
```

### `count_valid(df, unblind_codes)`

Counts rows in a findings data frame, optionally excluding unblinding-sensitive
records (negative subject IDs where no unblinding code is present).

```r
n <- count_valid(df)
n <- count_valid(df, unblind_codes = c("HBA1C", "GLUCOSE"))
```

### `create_project(name, path, overwrite)`

Scaffolds a new project folder structure and copies all templates.

```r
rCoreGage::create_project("TRIAL_ABC", path = "C:/Projects")
rCoreGage::create_project("TRIAL_ABC", path = "C:/Projects", overwrite = TRUE)
```

***

## 12. Console Output Reference

```
========== CoreGage : Starting Run ==========
  Project : TRIAL_ABC

>> [setup] Starting CoreGage initialisation ...
  Sheet 'Trial' columns : Category, Subcategory, ID, Active, ...
  Sheet 'Trial' rows    : 33
  Sheet 'Study' rows    : 33
  Active: 66 ON  /  0 OFF

>> Loading domain inputs from: C:/.../inputs
   AE.csv  -> domains$ae   (81 rows)
   LB.csv  -> domains$lb   (1003 rows)
   Domains loaded: AE, CM, DD, DM, DS, DV, EC, EX, FA, LB, MH, PC, PP, SV, VS

>>>>>>>>>>>>>>>>>>>>>> Executing: AE <<<<<<<<<<<<<<<<<<<<
  Running AECHK001 - AE end date before start date ...
  >> [collector] Appending 5 finding(s) for: AECHK001
  Running AECHK002 - Missing AESEV ...
  >> [collector] Appending 3 finding(s) for: AECHK002
>>>>>>>>>>>>>>>>>>>>>> Finished:  AE <<<<<<<<<<<<<<<<<<<<

>> [runner] All checks executed. Total findings: 1730

>> [reporter] Starting consolidation ...
  Importing saved issues: all_open.xlsx
    -> 1710 saved issue(s) loaded from all_open.xlsx
  Importing feedback from: feedback/DM/ (1 file(s))
    -> 1710 finding(s) matched from feedback/DM/
         status updates: 3  |  analyst notes: 1  |  review notes: 1
  -------------------------------------------------------
  Feedback summary:
    Notes  : analyst notes: 2  |  reviewer notes: 2
    Status : open: 1726  |  queried: 1  |  closed: 3
  -------------------------------------------------------
  Writing: DM_issues.xlsx
  Writing: MW_issues.xlsx
  Writing: SDTM_issues.xlsx
  Writing: ADAM_issues.xlsx
  Writing: all_open.xlsx
  Writing: all_closed.xlsx
>> [reporter] All reports written to: C:/.../outputs/reports

========== CoreGage : Run Complete ==========
>> Reports: C:/.../outputs/reports
```

***

## 13. Features

### Configuration-driven

All checks are registered in `rule_registry.xlsx`. Switching a check on or off
is a one-cell change (`Active = No`). No code editing required.

### Auto-discovery of domain data

Drop a new `.csv` into `inputs/` and it is loaded automatically as
`domains$<name>`. No code change required.

### Role-based report routing

Each check is independently tagged for four report channels. A single finding
can appear in multiple reports simultaneously based on the `DM_Report` for data management,
`MW_Report`for medical writing, `SDTM_Report` for SDTM programmer, `ADAM_Report` for ADAM programmer flags in the registry.

### Persistent feedback loop

Reviewer notes, analyst notes, and status changes are preserved across runs.
The most recently modified feedback file per role is used when multiple files
exist in a feedback folder.

### Smart status management

- Findings that disappear from the data are automatically closed with a note
- Findings that were closed by a reviewer remain closed on future runs
- Findings that re-appear after analyst closure are automatically re-opened
- Duplicate auto-tags are never appended twice

### Unblinding protection

Checks can optionally exclude records where the subject ID is negative and the
description does not contain a specified topic code — preventing accidental
unblinding of sensitive records in outputs.

### CRAN-compatible package structure

Engine functions are documented with roxygen2, exported via NAMESPACE, and
covered by testthat unit tests. The package ships templates and the blank
rule registry in `inst/` for distribution with `system.file()`.

### Cross-platform

Works on Windows, macOS, and Linux. File paths use `file.path()` throughout.
Date parsing handles ISO strings, Excel serial numbers, and `ddMMMYYYY` format
without platform-specific issues.

***

## 14. Examples

### Example 1 — Minimal date check

```r
# rules/trial/AE.R
check_AE <- function(state, cfg) {
  domains      <- state$domains
  active_rules <- state$active_rules

  if (is.null(domains$ae) || nrow(domains$ae) == 0) return(state)
  ae <- domains$ae

  if (isTRUE(active_rules["AECHK001"])) {
    AECHK001 <- ae |>
      dplyr::filter(!is.na(AESTDTC), !is.na(AEENDTC)) |>
      dplyr::mutate(st = as.Date(AESTDTC), en = as.Date(AEENDTC)) |>
      dplyr::filter(en < st) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = paste0("End (", format(en, "%d%b%Y"), ") before start (",
                             format(st, "%d%b%Y"), ") for: ", AETERM)
      ) |>
      dplyr::select(subj_id, vis_id, description)
    state <- collect_findings(state, AECHK001, id = "AECHK001")
  }
  state
}
```

### Example 2 — Cross-domain study-level check

```r
# rules/study/DM_study.R
# Check that all subjects in AE also exist in DM
check_DM_study <- function(state, cfg) {
  domains      <- state$domains
  active_rules <- state$active_rules

  if (isTRUE(active_rules["DMPRJ004"])) {
    ae_subjects <- unique(domains$ae$USUBJID)
    dm_subjects <- unique(domains$dm$USUBJID)

    DMPRJ004 <- data.frame(
      subj_id = setdiff(ae_subjects, dm_subjects),
      stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(
        vis_id      = NA_real_,
        description = paste0("Subject ", subj_id,
                             " has AE records but no DM record")
      ) |>
      dplyr::select(subj_id, vis_id, description)

    state <- collect_findings(state, DMPRJ004, id = "DMPRJ004")
  }
  state
}
```

### Example 3 — Range check with normal limits

```r
# rules/trial/LB.R  (LBCHK001)
if (isTRUE(active_rules["LBCHK001"])) {
  LBCHK001 <- lb |>
    dplyr::filter(!is.na(LBORRES), !is.na(LBNRLO), !is.na(LBNRHI)) |>
    dplyr::mutate(
      val = suppressWarnings(as.numeric(LBORRES)),
      lo  = suppressWarnings(as.numeric(LBNRLO)),
      hi  = suppressWarnings(as.numeric(LBNRHI))
    ) |>
    dplyr::filter(!is.na(val), val < lo | val > hi) |>
    dplyr::mutate(
      subj_id     = USUBJID,
      vis_id      = as.numeric(VISITNUM),
      description = paste0(LBTEST, " = ", val, " ", LBORRESU,
                           " outside [", lo, " - ", hi, "]",
                           " at visit ", VISIT)
    ) |>
    dplyr::select(subj_id, vis_id, description)
  state <- collect_findings(state, LBCHK001, id = "LBCHK001")
}
```

### Example 4 — Disable a check without deleting it

In `rule_registry.xlsx`, set `Active = No` for the check row. It will be
skipped silently on every run. Set it back to `Yes` to re-enable.

### Example 5 — Add a new domain

```
1. Drop DM_SUPP.csv into inputs/
   → automatically loaded as domains$dm_supp

2. Create rules/trial/DM_SUPP.R  with  check_DM_SUPP(state, cfg)

3. Add rows to rule_registry.xlsx (Trial sheet):
   ID=DMSCHK001  Rule_Set=DM_SUPP  Active=Yes  ...

4. source("run_coregage.R")
```

No engine code changes required.

***

## 15. Troubleshooting

### `WARNING: cannot read all_open.xlsx`

The file may be open in Excel. Close it and re-run. Excel locks `.xlsx` files
while they are open, preventing R from reading them.

### `Function 'check_AE' not found in AE.R`

The function name inside the script does not match the `Rule_Set` value.
If `Rule_Set = "AE"`, the function must be named exactly `check_AE`.

### `rule_registry.xlsx not found`

The `cfg$rule_registry` path is incorrect. Open `rules/config/project_config.R`
and verify the path. The most common cause is opening the script directly
instead of via the `.Rproj` file (which sets `getwd()` correctly).

### Feedback not carried forward

1. Confirm the feedback `.xlsx` file is in the correct subfolder
   (`feedback/DM/`, not `feedback/` directly)
2. Confirm the file is not open in Excel when running
3. Check the console for `0 rows could be read` — this means the file
   is locked or has an unexpected structure
4. The Details sheet must have headers matching the standard column names
   (`CHECK ID`, `SUBJECT ID`, `ANALYST NOTE` etc.)

### `[Was closed but re-appeared]` appearing unexpectedly

This means a finding that was previously marked `Closed` by the analyst
(with `closed by analyst` in the note) has reappeared in the current run's
data. If the closure was done by a reviewer, the tag will not appear —
reviewer-closed findings are tracked separately via `[closed by reviewer]`.

### `.sas7bdat files found but 'haven' not installed`

```r
install.packages("haven")
```

***

## 16. Contributing

Contributions are welcome. Please open an issue first to discuss proposed changes.

Bug Reports & Feature Requests
Found a bug or have an idea to improve `rCoreGage`?
Please raise it via the **[GitHub Issues](https://github.com/ganeshbabunn/rCoreGage/issues)** tab.

When opening a new issue, use the appropriate label:

|Type     |Label   |When to use|
|-------- |--------|------------|
| Bug     | `bug`         | Something is broken or behaving unexpectedly   |
| Feature | `enhancement` | A new feature or improvement you'd like to see |

### Steps to raise an issue

1. Go to the **[Issues tab](https://github.com/ganeshbabunn/rCoreGage/issues)**
2. Click **"New Issue"**
3. Choose the relevant template — **Bug Report** or **Feature Request**
4. Fill in the details and apply the correct label (`bug` or `enhancement`)
5. Click **"Submit new issue"**

> **Tip:** Before opening a new issue, please search existing issues to avoid duplicates.

### Sample Study has been setup

rCoreGage\inst\STUDY01_TRAIL01\

Covered few project(study) and trial related checks has been covered with all sample data and scripts available in the above locations.
This has been tested with all the scenario.

***

## License

GPL (>= 3) © Ganesh Babu G

***

## Citation

```
rCoreGage: Data Quality Check Framework for Clinical and Analytical Data. 
https://github.com/ganeshbabuNN/rCoreGage
```
