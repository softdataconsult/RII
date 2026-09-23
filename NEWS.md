# RII 0.1.0

Initial release.

* `rii_raw()`, `rii_freq()` — single-item RII, mean and SD from raw scores
  or a frequency count, on any scale range (`scale_min`/`scale_max`), with
  optional reverse-scoring (`reverse = TRUE`) for negatively-worded items.
* `reverse_score()` — standalone helper for reverse-scoring Likert data.
* `rii_table()`, `rii_table_freq()` — ranked RII tables from raw
  respondent-level data or a frequency table, with `reverse_items` support.
* `read_rii_excel()` — read either layout straight from an `.xlsx`
  workbook via readxl.
* `read_rii_csv()` — same, for `.csv` files (base R only, no extra
  dependency).
* `read_rii_spss()` — same, for SPSS `.sav` files via haven, automatically
  converting SPSS's labelled values to plain numeric scores.
* `write_rii_excel()` — write a ranked table to a formatted `.xlsx`
  workbook via openxlsx, with colour-coded importance bands and the
  ranked chart embedded by default — the mirror of `read_rii_excel()`.
* `classify_rii()` — Low/Medium-Low/Medium/High-Medium/High importance
  bands.
* `print.rii_table()`, `summary.rii_table()`, `plot.rii_table()` methods.
* Bundled example datasets:
  - `building_collapse_nigeria`: 150 simulated Nigerian construction
    professionals rating ten causes of building collapse (5-point scale).
  - `csa_adoption_barriers_nigeria`: 175 simulated Nigerian farmers rating
    ten barriers to climate-smart agriculture adoption (4-point scale) —
    a non-construction example showing the package works for any domain
    and any scale range.
* "Getting Started with RII" vignette.
* `plot.rii_table()` automatically shortens item labels and shrinks text
  in small plotting windows instead of failing with base R's
  "figure margins too large" error; `max_label_chars` and `cex.names`
  let you control this directly if needed.
* `detect_likert_items()` — scans a raw survey export (with demographic/ID
  columns mixed in) and suggests which columns are actually Likert items,
  using column-name conventions and data patterns; `rii_table(data,
  items = "auto")` uses it directly as a one-line entry point for messy
  real-world data.
* `rii_table()` (and therefore `read_rii_excel()`/`read_rii_csv()`/
  `read_rii_spss()`) now accepts `items` as a numeric vector of column
  positions (e.g. `items = 11:17`), not just column names or `"auto"` —
  useful for selecting one section/construct out of a longer
  questionnaire.
* Fixed `detect_likert_items()`'s name-pattern matching to recognize a
  code followed by more text (e.g. `Q1_NotifyDuringLecture`), not just an
  exact short code (`OJ1`) — it previously missed real-world column names
  that combine a short code with a descriptive suffix.
* `recode_likert()` / `detect_text_likert_items()` — convert text Likert
  labels ("Strongly Agree", "Disagree", ...) to numeric scores, with
  auto-detection of which columns use a recognized wording (agreement,
  frequency, satisfaction, importance, extent scales built in, or supply
  your own). `rii_table(items = "auto")` picks up recoded columns
  automatically. `read_rii_csv()`/`read_rii_excel()`/`read_rii_spss()`
  all take a `recode_text = TRUE` argument to do this in the same call
  as reading the file.
