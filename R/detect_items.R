#' Detect likely Likert-item columns in a raw data frame
#'
#' The biggest friction point in using \code{\link{rii_table}} on a real
#' survey export isn't the RII math — it's figuring out which of 30+
#' columns are actually the Likert-scale items to rank, versus
#' demographic/ID columns that happen to also be small numbers (gender
#' coded 1/2, marital status coded 1/2/3, and so on look, numerically,
#' just as "Likert-like" as a real item). \code{detect_likert_items()}
#' scans every column and suggests which ones are items, using two
#' signals: whether the column name follows the short-code-plus-number
#' convention common in survey instruments (\code{OJ1}, \code{PJ6},
#' \code{JS1i}, ...), and whether the name contains a common
#' demographic/ID keyword (age, gender, marital, id, date, ...). Columns
#' that are almost entirely missing, or have no variation at all, are
#' flagged regardless of the name, since these are usually data-entry
#' artifacts rather than real items.
#'
#' This is a starting point to review, not a silent decision — always
#' check the \code{Reason} column before passing the suggested items into
#' \code{\link{rii_table}}.
#'
#' @param data A data frame — typically a raw survey export with a mix of
#'   demographic/ID columns and Likert-item columns.
#' @param item_pattern A regular expression a column name is checked
#'   against to see if it looks like an item code (default: 1-6 letters
#'   followed by digits, at the start of the name — matches conventions
#'   like \code{OJ1}, \code{JS2iii}, or \code{Q1_NotifyDuringLecture},
#'   since anything can follow the code).
#' @param exclude_keywords Character vector of keywords (matched
#'   case-insensitively, as substrings) that mark a column as
#'   demographic/ID rather than an item, e.g. \code{"gender"},
#'   \code{"age"}, \code{"id"}. See the function default for the full
#'   list; pass your own to adapt it to a different survey's column
#'   naming.
#' @param max_na_pct Columns with more than this percentage of missing
#'   values are flagged as likely data-entry artifacts rather than real
#'   items, regardless of name. Defaults to 50.
#'
#' @return A data frame (class \code{"likert_item_scan"}, with a
#'   \code{print} method) with one row per column of \code{data}:
#'   \code{Column}, \code{N_Unique}, \code{Min}, \code{Max}, \code{Pct_NA},
#'   \code{Suggested} (logical — our best guess) and \code{Reason}.
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   `S/N` = 1:20, GENDER = sample(1:2, 20, TRUE),
#'   OJ1 = sample(1:5, 20, TRUE), OJ2 = sample(1:5, 20, TRUE),
#'   PJ6 = sample(1:5, 20, TRUE), check.names = FALSE
#' )
#' scan <- detect_likert_items(d)
#' scan
#' items <- scan$Column[scan$Suggested]
#' items
#'
#' @export
detect_likert_items <- function(
  data,
  item_pattern = "^[A-Za-z]{1,6}[0-9]+",
  exclude_keywords = c(
    "s/n", "sn", "serial", "id", "name", "date", "age", "sex", "gender",
    "marital", "education", "qualification", "heq", "hqe", "experience",
    "tenure", "service", "status", "department", "dept", "unit",
    "faculty", "school", "state", "lga", "religion", "tribe", "ethnic",
    "income", "salary", "phone", "email", "address", "occupation",
    "designation", "rank", "grade", "level", "class", "year", "yr"
  ),
  max_na_pct = 50
) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }

  cols <- names(data)
  n_unique <- vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1))
  is_num   <- vapply(data, is.numeric, logical(1))
  min_v    <- vapply(data, function(x) if (is.numeric(x) && any(!is.na(x))) min(x, na.rm = TRUE) else NA_real_, numeric(1))
  max_v    <- vapply(data, function(x) if (is.numeric(x) && any(!is.na(x))) max(x, na.rm = TRUE) else NA_real_, numeric(1))
  pct_na   <- vapply(data, function(x) 100 * mean(is.na(x)), numeric(1))

  matches_pattern <- grepl(item_pattern, trimws(cols))
  matches_keyword <- vapply(cols, function(cn) {
    cn_l <- tolower(cn)
    any(vapply(exclude_keywords, function(k) grepl(k, cn_l, fixed = TRUE), logical(1)))
  }, logical(1))

  reason <- character(length(cols))
  suggested <- logical(length(cols))

  for (i in seq_along(cols)) {
    if (!is_num[i]) {
      reason[i] <- "not numeric"
      suggested[i] <- FALSE
    } else if (pct_na[i] > max_na_pct) {
      reason[i] <- sprintf("%.0f%% missing - likely a data-entry artifact", pct_na[i])
      suggested[i] <- FALSE
    } else if (n_unique[i] < 2) {
      reason[i] <- "no variation (same value for every respondent)"
      suggested[i] <- FALSE
    } else if (matches_keyword[i]) {
      reason[i] <- "name matches a demographic/ID keyword"
      suggested[i] <- FALSE
    } else if (matches_pattern[i]) {
      reason[i] <- "name matches item-code pattern (e.g. OJ1, JS2iii)"
      suggested[i] <- TRUE
    } else {
      reason[i] <- "name doesn't match the item-code pattern"
      suggested[i] <- FALSE
    }
  }

  out <- data.frame(
    Column    = cols,
    N_Unique  = n_unique,
    Min       = min_v,
    Max       = max_v,
    Pct_NA    = round(pct_na, 1),
    Suggested = suggested,
    Reason    = reason,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  class(out) <- c("likert_item_scan", "data.frame")
  out
}

#' Print a likert_item_scan
#'
#' @param x An object of class \code{"likert_item_scan"}.
#' @param ... Further arguments (currently unused).
#' @export
print.likert_item_scan <- function(x, ...) {
  n_sugg <- sum(x$Suggested)
  cat(sprintf("%d of %d columns suggested as Likert items\n", n_sugg, nrow(x)))
  cat(strrep("-", 40), "\n", sep = "")
  print.data.frame(as.data.frame(unclass(x)), row.names = FALSE)
  invisible(x)
}
