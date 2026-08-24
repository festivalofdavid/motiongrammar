# motiongrammar <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- badges: end -->

## Overview

- **What it does**: takes tracking data (GPS, RFID, optical) from csv, gpx files and API's, processes it through a structured nine-step pipeline, returning tidy summary tables.
- **Goal**: processing tracking data is currently disjointed and relies on many packages. Here, we unite these complex into a tidy pipeline that is easier to learn and implement.
- **Core object**: we view tracking data as a `motion_trace` class. This is a tibble with two extra attributes, `metadata` (device/session info) and `quality` (provenance log), that survive every operation. This is a key boon for reproducibility in the academic world, as we can save every operation performed on the data through to the end.
- **Philosophy** — tidyverse; every function accepts a `motion_trace` on the left and returns one on the right, so the entire pipeline chains with `|>`

---

## The pipeline

```
initiate → coordinate → interpolate → filtrate → derivate →
designate → elaborate → allocate → quantitate
```

```r
library(motiongrammar)

exemplar_trace <- initiate(session = "gps_session.csv",
                           template = load_csv_template("my_device.xml")) |>
  coordinate(norm = TRUE) |>
  interpolate(hz = 10, max_gap_frames = 20) |>
  filtrate(method = "butterworth", target = "coordinates", cutoff = 0.4) |>
  derivate(derivatives = c("velocity", "acceleration", "angular_velocity")) |>
  designate(method = "manual", file = "drill_labels.csv") |>
  elaborate(velocity = pmin(velocity, 8.5),
            curved_load = velocity * angular_velocity) |>
  allocate_csv(file = "intensity_bands.csv") |>
  quantitate(allocation = "gps_standard", derivative = "velocity",
             scope = "designation", time_s = n() / 10)
```

---

## Installation

```r
# Development version
devtools::install_github("festivalofdavid/motiongrammar")
```

**Hard dependencies (installed automatically):** dplyr, tibble, readr, stringr, lubridate, sf, zoo, changepoint, httr2, jsonlite, purrr, stringdist, tidyr, uuid, rlang, vctrs

**Optional:** signal (Butterworth filtering via `filtrate(method = "butterworth")`), ggplot2, patchwork

---

## Step-by-step

### 1. `initiate()` — load data

- Reads GPS (CSV, GPX), RFID, optical, Strava, or any generic HTTP API stream
- Returns a `motion_trace` with `unix_time`, coordinate columns, and any extra sensor columns (heart rate, player load, cadence, etc.)
- `source = 'auto'` detects file type; `source = 'guess_csv'` fuzzy-detects column names for a quick look at an unfamiliar file
- **Templates** lock in a device's column mapping so ingestion is identical across sessions:
  - `csv_template()` — define column mapping, skip rows, coord system
  - `guess_csv_template()` — auto-detect from a file, then refine
  - `save_csv_template()` / `load_csv_template()` — persist as XML
  - `api_stream_template()` — same pattern for HTTP APIs
- `set_metadata()` — attach player name, session date, sport, or any custom field
- Supported coordinate systems: GPS (`lat`/`lng`), local XY, local XYZ
- Quality log records: row count, column completeness, temporal gap count, duplicate timestamps, detected Hz, device metadata
- NB -- Strava has changed their API since this package was written. The current process should work in the meantime, but I am still working out how to incorporate their new model.
### 2. `coordinate()` — project from spherical system to xy, and between imperial and metric systems

- Converts spherical GPS degrees to a flat Cartesian plane (metres)
- `norm = TRUE` — translates origin to the first valid position
- Supports manual origin: supply a reference lat/lng to anchor all sessions to the same physical point
- `drop_outliers = TRUE` — removes position jumps that exceed a speed threshold before projection
- Adds `x`, `y`, and optionally `z` columns; preserves `lat`/`lng` originals
- Quality log records: CRS used, origin coordinates, outlier rows removed, bounding box

### 3. `interpolate()` — regularise frame rate

- Builds a dense time grid at the target `hz` and joins the raw data onto it
- Fills short gaps (`<= max_gap_frames`) with the chosen method; gaps beyond that threshold stay `NA`
- Methods: `'linear'` (default, piecewise), `'spline'` (cubic — smoother but can overshoot), `'constant'` (LOCF)
- **Numeric passthrough columns** (heart rate, cadence, etc.) are interpolated with the same method; character/factor columns are left as-is with `NA` for inserted rows
- Adds `is_interpolated` (logical) column — marks every row that was filled rather than observed
- Quality log records: gaps found, gap lengths, % rows interpolated, NAs filled per coordinate, passthrough columns interpolated vs skipped

### 4. `filtrate()` — noise attenutation

- Applies a digital filter to remove measurement noise from position or derivative signals
- `target = 'coordinates'` filters `x`/`y`; `target = 'derivatives'` filters velocity, acceleration, angular_velocity
- Methods: `'butterworth'` (requires signal package), `'moving_average'`, `'savitzky_golay'`
- Filtered signal stored in `f_x`/`f_y` columns; originals untouched — downstream steps use filtered by default
- `filtrate_cutoff()` — diagnostic: sweeps a cutoff range and plots residuals to find the elbow; returns suggested cutoff
- Quality log records: method, cutoff used, pass number (multiple passes supported)
- NB: many vendor files are pre-filtered; if the residual plot shows no elbow, skip this step

### 5. `derivate()` — compute derivatives

- Computes time-series derivatives using central differences
- Available derivatives: `'velocity'` (m/s), `'acceleration'` (m/s²), `'angular_velocity'` (deg/s), `'jerk'` (m/s³), `'lateral_g'` (experimental centripetal proxy)
- `use_filtered = TRUE` (default) — uses `f_x`/`f_y` when available; falls back to `x`/`y`
- `speed_floor` — velocity below this threshold is treated as stationary; heading/omega are zeroed to suppress sensor jitter artefacts
  - `lateral_g` is explicitly forced to 0 when velocity < speed_floor
- `window` — rolling window size per derivative (e.g. `list(velocity = 5, acceleration = 10)`)
- `signed_omega = TRUE` — angular velocity is signed (positive = left turn); default is unsigned magnitude
- Quality log records: window sizes, speed_floor, which derivatives computed, column-level min/max/mean/NA counts

### 6. `designate()` — segment the session

- Labels every row with a segment name (`active_1`, `relief_1`, etc.)
- **`method = 'corbett'`** (default) — PELT changepoint detection on the velocity signal; `v_threshold` splits active from relief
  - `export_designations()` — writes detected segment ranges to CSV as a named skeleton for manual editing
- **`method = 'manual'`** — reads a three-column CSV (`designation`, `unix_start`, `unix_end`); `cols = 'guess'` handles non-standard column names
- NA velocity rows (interpolated gaps) are propagated forward/backward so no row is left undesignated
- Quality log records: method, source file (manual), changepoint indices and segment lengths (corbett), % rows designated

### 7. `elaborate()` — add custom time-series

- `motion_trace`-safe `dplyr::mutate()` wrapper — preserves `metadata` and `quality` attributes that bare `mutate()` drops
- Accepts any number of named expressions: `elaborate(curved_load = velocity * angular_velocity)`
- `log_expr = TRUE` — stores the deparsed expression for every column in the quality log; critical when overwriting reserved pipeline columns (`velocity`, `acceleration`, etc.) since downstream functions use those columns silently
- Quality log flags with `⚠ Overwrote` when a reserved pipeline column is replaced

### 8. `allocate()` / `allocate_csv()` — assign intensity bands

- Maps each row's derivative value to an intensity band based on user-defined thresholds
- `allocate()` — programmatic: pass a named list of threshold vectors
- `allocate_csv()` — reads a CSV of band definitions; supports multiple named profiles in one file
- Multiple allocations can coexist on the same trace under different names (e.g. `'gps_standard'`, `'custom_session'`)
- Adds `{allocation_name}_velocity_band`, `{allocation_name}_acceleration_band`, etc. columns
- Reference allocations by name in `quantitate(allocation = 'gps_standard')`
- Quality log records: threshold boundaries, band names, rows per band

### 9. `quantitate()` — summarise

- Aggregates a `motion_trace` by designation segment and intensity band; returns a plain tibble
- `scope`:
  - `'designation'` — one row per designation × band combination
  - `'session'` — one row per band across the whole session
  - `'active'` — session-level but restricting to `active_*` segments only
- `active_only = TRUE` (with `scope = 'designation'`) — drops relief segments before aggregating
- Accepts arbitrary `dplyr::summarise()` expressions: `time_s = n() / 10, peak_vel = max(velocity, na.rm = TRUE)`
- `band_duration()` — convenience wrapper; computes `duration_sec` per band using `interpolation_hz` from metadata
- Full quality log is attached to the result tibble as `attr(result, 'quality')` — the summary table is self-describing
- **Note:** `write_csv` silently drops attributes; use `saveRDS` to preserve provenance

---

## The quality log

- Every pipeline step appends an entry to `attr(trace, 'quality')`
- `quality_report(trace)` — prints a summary of all steps applied so far
- `quality_report(trace, step = 'interpolate')` — inspect a single step
- `metadata_report(trace)` — prints device/session metadata
- `full_report(trace)` — prints both together
- Each entry records: step name, UUID, package version, UTC timestamp, rows/cols before and after, step-specific diagnostics, and any issues detected
- The log survives every pipeline operation and is copied onto `quantitate()` output
- Repeated calls to the same pipeline step append rather than overwrite — run count is shown in the report

---

## Data persistence

```r
# Keep full object with all attributes
saveRDS(trace, "session_processed.rds")

# Summary tables are safe as CSV
readr::write_csv(by_vel, "by_velocity.csv")

# Save quality log separately
saveRDS(attr(trace, "quality"), "quality_log.rds")
```

---

## Supported data sources

| Source | `source =` argument | Notes |
|---|---|---|
| Generic CSV with template | `'csv'` or `'auto'` | Preferred production path |
| Auto-detect CSV | `'guess_csv'` | For exploration only |
| GPX file | `'gpx'` | Requires sf package |
| Strava activity | `'strava'` | Requires OAuth tokens |
| Generic HTTP API | `'generic'` | Requires `api_stream_template()` |

---

## Getting help

- `?initiate`, `?quality_report`, etc. for function docs
- `vignette("quickstart", package = "motiongrammar")` for end-to-end walkthrough
- File issues at <https://github.com/festivalofdavid/motiongrammar/issues>
