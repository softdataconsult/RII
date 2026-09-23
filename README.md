# RII

<!-- badges: start -->
[![R-CMD-check](https://github.com/softdataconsult/RII/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/softdataconsult/RII/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Relative Importance Index (RII) for Likert-scale survey data — in R
instead of a spreadsheet.**

RII is the standard tool for ranking factors, causes or criteria rated by
respondents on a Likert scale, widely used in environmental studies,
construction management, engineering and social science research:

```
RII = (sum of respondent weights) / (highest weight x number of respondents)
```

It gives every item a score between 0 and 1, letting you rank "causes of
X" or "barriers to Y" from most to least important. Most researchers
currently compute it by hand in Excel, one `SUMPRODUCT` formula per row.
This package does the same calculation in a couple of lines of R —
reading directly from Excel, CSV or SPSS, auto-detecting item columns
even in messy real-world exports, and producing a ranked table, a chart,
and a formatted Excel report — so it's a gentle first project for
researchers moving from Excel to R.

## Contents

* [Installation](#installation)
* [Quick start](#quick-start)
* [Messy real-world data](#messy-real-world-data-demographics-mixed-in-text-labels-multiple-sections)
* [Reading and writing files directly](#reading-and-writing-files-directly)
* [Any scale, reverse-scoring](#any-likert-scale-range-and-reverse-scoring)
* [Why this instead of the spreadsheet?](#why-this-instead-of-the-spreadsheet)
* [Bundled example data](#bundled-example-data)
* [Citing this package](#citing-this-package)
* [Learn more](#learn-more)
* [Contact & Support](#contact--support)

## Installation

```r
install.packages("remotes")
remotes::install_github("softdataconsult/RII")
```

## Quick start

```r
library(RII)

# Bundled example: 150 Nigerian construction professionals rating
# 10 perceived causes of building collapse on a 5-point scale
data(building_collapse_nigeria)

items <- building_collapse_nigeria[, 5:14]   # just the Likert columns
tbl <- rii_table(items)
tbl
#> Relative Importance Index table (scale 1-5)
#> ----------------------------------------
#>                         Item   N  Mean    SD   RII Rank        Band
#>         SubstandardMaterials 150 4.720 0.569 0.944    1        High
#>    NonComplianceBuildingCode 150 4.647 0.657 0.929    2        High
#>              PoorWorkmanship 150 4.633 0.727 0.927    3        High
#>              PoorSupervision 150 4.607 0.713 0.921    4        High
#>          UnqualifiedArtisans 150 4.493 0.841 0.899    5        High
#>    CorruptionApprovalProcess 150 4.480 0.865 0.896    6        High
#>       StructuralDesignErrors 150 4.140 1.232 0.828    7        High
#>  InadequateSoilInvestigation 150 4.027 1.152 0.805    8        High
#>                  Overloading 150 3.727 1.263 0.745    9 High-Medium
#>            LackOfMaintenance 150 3.353 1.391 0.671   10 High-Medium

summary(tbl)   # top item, RII range, counts per importance band
plot(tbl)      # ranked horizontal bar chart
```

If you only have a frequency count per scale point (the layout of most
spreadsheet RII templates) rather than raw responses, use
`rii_table_freq()` instead — see `?rii_table_freq`.

## Messy real-world data: demographics mixed in, text labels, multiple sections

Real survey exports rarely look like the tidy example above. A few things
are handled directly:

**Demographic/ID columns mixed in with items.** A gender column coded 1/2
looks numerically just as "Likert-like" as a real item. Rather than
typing out every item column name by hand, `items = "auto"` detects them
from the column-naming convention (`OJ1`, `PJ6`, `Q1_NotifyDuringLecture`,
...):

```r
rii_table(my_raw_survey_data, items = "auto")
```

It prints exactly what it included/excluded so nothing is silent — run
`detect_likert_items(my_data)` yourself first if you want to review or
adjust the guess before trusting it.

**Text Likert labels instead of numbers** — "Strongly Agree", "Disagree",
and so on (common in Google Forms exports). Convert them first with
`recode_likert()`:

```r
d2 <- recode_likert(my_raw_survey_data)   # auto-detects which columns to recode
rii_table(d2, items = "auto")             # picks up exactly those columns
```

Built-in wordings cover agreement, frequency, satisfaction, importance and
extent scales; pass your own `mapping = c("no" = 1, "yes" = 2)` for
anything else.

**Multiple constructs/sections in one questionnaire.** Different sections
(e.g. "Road Surface Quality" vs "Maintenance Practices") should usually be
ranked separately. `items` accepts a numeric column range for this:

```r
d <- read.csv("survey.csv")
road_quality <- rii_table(d, items = 4:10)    # Section 1
maintenance  <- rii_table(d, items = 11:13)   # Section 2
```

## Reading and writing files directly

`.xlsx`, `.csv`, and SPSS `.sav` files are all supported directly — same
two layouts (`"raw"` respondent-level data or `"freq"` frequency tables),
same `items = "auto"` detection, and `recode_text = TRUE` to handle text
Likert labels in the same call:

```r
read_rii_excel("survey.xlsx", layout = "raw", items = "auto")
read_rii_csv("survey.csv", layout = "raw", items = "auto")
read_rii_spss("survey.sav", layout = "raw", items = "auto")             # requires haven
read_rii_csv("survey.csv", layout = "raw", items = "auto",
              recode_text = TRUE)                                       # text labels
```

`read_rii_spss()` automatically strips SPSS's value labels (e.g. `1`
labelled `"Strongly Disagree"`) down to the plain numeric score.

To hand results back to someone as a spreadsheet — colour-coded importance
bands and the chart included — write the ranked table straight to `.xlsx`:

```r
write_rii_excel(tbl, "rii_results.xlsx")
```

## Any Likert scale range, and reverse-scoring

Not every survey uses a 1-5 scale. `scale_min`/`scale_max` handle any
range (0-based, 3-point, 7-point, ...), and `reverse_items` reverse-scores
negatively-worded items before ranking:

```r
rii_table(data, scale_min = 0, scale_max = 4,
          reverse_items = c("PoorlyWordedItem1", "PoorlyWordedItem2"))
```

## Why this instead of the spreadsheet?

* One function call replaces one `SUMPRODUCT`/`COUNTIF` formula per item,
  copied down a column.
* Ranking, mean, SD and RII are computed together and kept in sync — no
  risk of a formula pointing at the wrong row after a sort.
* Item columns, text labels, and messy real-world layouts are handled
  automatically instead of by hand.
* `classify_rii()` applies the standard Low/Medium-Low/Medium/High-Medium/High
  bands used in the literature automatically.
* `plot(tbl)` gives a publication-style ranked bar chart with one line of
  code, using only base R graphics (no extra packages required for the
  core calculation).

## Bundled example data

* `building_collapse_nigeria` — 150 construction professionals rating
  causes of building collapse on a 5-point scale.
* `csa_adoption_barriers_nigeria` — 175 farmers rating barriers to
  climate-smart agriculture adoption on a **4-point** scale, showing RII
  applies just as well outside construction and on non-5-point scales.

```r
library(RII)

data(csa_adoption_barriers_nigeria)
barriers <- csa_adoption_barriers_nigeria[, 6:15]
rii_table(barriers, scale_max = 4)
```

## Citing this package

```r
citation("RII")
```

```
Ajao I (2026). RII: Relative Importance Index for Likert-Scale
Survey Data. R package version 0.1.0,
<https://github.com/softdataconsult/RII>.
```

GitHub also shows a "Cite this repository" button (top-right of the repo
page) for an APA-style citation or BibTeX, generated from `CITATION.cff`.

## Learn more

* `?rii_table`, `?rii_table_freq` — the two main entry points
* `?rii_raw`, `?rii_freq` — the underlying single-item calculations, if
  you want to build something custom
* `?detect_likert_items`, `?recode_likert`, `?detect_text_likert_items` —
  handling messy real-world survey data
* `?read_rii_excel`, `?read_rii_csv`, `?read_rii_spss`, `?write_rii_excel`
  — file import/export
* `?building_collapse_nigeria`, `?csa_adoption_barriers_nigeria` — the
  bundled example datasets
* `vignette("RII-intro", package = "RII")` — worked walkthrough

## Contact & Support

* **Bug reports and feature requests:** [GitHub Issues](https://github.com/softdataconsult/RII/issues)
* **Email:** softdataconsult@gmail.com
* **Ekiti R-Users Group:** [YouTube (@softdataconsult)](https://www.youtube.com/@softdataconsult)

## Author

Developed by Dr. Isaac O. Ajao ([ORCID](https://orcid.org/0000-0002-3403-6082),
SoftData Consult) for the Ekiti R-Users Group, to help environmental,
construction and social science researchers already using RII in Excel
make the move to R.
