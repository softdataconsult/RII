# Built-in text-label dictionaries for common Likert scale wordings.
# Not exported; used by default in recode_likert() / detect_text_likert_items().
.likert_dictionaries <- list(
  agreement5 = c(
    "strongly disagree" = 1, "disagree" = 2, "neutral" = 3,
    "agree" = 4, "strongly agree" = 5
  ),
  agreement5_undecided = c(
    "strongly disagree" = 1, "disagree" = 2, "undecided" = 3,
    "agree" = 4, "strongly agree" = 5
  ),
  agreement4 = c(
    "strongly disagree" = 1, "disagree" = 2, "agree" = 3, "strongly agree" = 4
  ),
  frequency5 = c(
    "never" = 1, "rarely" = 2, "sometimes" = 3, "often" = 4, "always" = 5
  ),
  satisfaction5 = c(
    "very dissatisfied" = 1, "dissatisfied" = 2, "neutral" = 3,
    "satisfied" = 4, "very satisfied" = 5
  ),
  importance5 = c(
    "not important" = 1, "slightly important" = 2, "moderately important" = 3,
    "important" = 4, "very important" = 5
  ),
  extent5 = c(
    "not at all" = 1, "a little" = 2, "somewhat" = 3,
    "quite a bit" = 4, "a great deal" = 5
  )
)

.apply_likert_mapping <- function(x, dict, ignore_case, trim, allow_na, col_name) {
  x_chr <- as.character(x)
  key <- x_chr
  if (trim) key <- trimws(key)
  if (ignore_case) key <- tolower(key)
  dict_key <- if (ignore_case) tolower(names(dict)) else names(dict)

  idx <- match(key, dict_key)
  out_num <- unname(dict[idx])
  unmapped <- unique(x_chr[is.na(idx) & !is.na(x_chr) & nzchar(trimws(x_chr))])

  if (length(unmapped) > 0) {
    if (allow_na) {
      warning("Column '", col_name, "': ", length(unmapped),
              " unmapped value(s) set to NA: ", paste(unmapped, collapse = ", "),
              call. = FALSE)
    } else {
      stop("Column '", col_name, "' has value(s) not covered by the mapping: ",
           paste(unmapped, collapse = ", "),
           ". Pass allow_na = TRUE to convert unmapped values to NA, or supply ",
           "a 'mapping' that covers them (e.g. mapping = c(\"strongly disagree\" ",
           "= 1, ..., \"strongly agree\" = 5, \"somewhat agree\" = 3.5)).",
           call. = FALSE)
    }
  }
  out_num
}

#' Recode text Likert labels to numeric scores
#'
#' Converts columns of text Likert responses — \code{"Strongly Agree"},
#' \code{"Disagree"}, and so on — to the numeric scores
#' \code{\link{rii_table}} needs. Matching is case-insensitive and ignores
#' leading/trailing whitespace by default, so \code{"Strongly Agree"},
#' \code{"strongly agree"} and \code{" Strongly Agree "} are all treated
#' the same.
#'
#' @param data A data frame containing one or more text Likert columns.
#' @param cols Character vector of column names in \code{data} to recode.
#'   Leave as \code{NULL} (the default) to have
#'   \code{\link{detect_text_likert_items}} find columns whose values
#'   match a known scale wording automatically — each such column is
#'   recoded using whichever built-in scale its own values matched (useful
#'   when a survey mixes an agreement scale for some questions with a
#'   frequency scale for others).
#' @param mapping Only used when \code{cols} is given explicitly (ignored
#'   when \code{cols = NULL}, since each column then uses its own detected
#'   scale). Either the name of a built-in scale — one of
#'   \code{"agreement5"} (Strongly Disagree..Strongly Agree),
#'   \code{"agreement5_undecided"} (...Undecided...),
#'   \code{"agreement4"} (no Neutral), \code{"frequency5"} (Never..Always),
#'   \code{"satisfaction5"}, \code{"importance5"}, or \code{"extent5"}
#'   (Not At All..A Great Deal) — or your own named numeric vector, e.g.
#'   \code{c("no" = 1, "maybe" = 2, "yes" = 3)}, for a wording not covered
#'   by the built-ins.
#' @param ignore_case Logical; match labels case-insensitively? Defaults
#'   to \code{TRUE}.
#' @param trim Logical; trim leading/trailing whitespace before matching?
#'   Defaults to \code{TRUE}.
#' @param allow_na Logical; if a value isn't covered by the mapping,
#'   convert it to \code{NA} with a warning (\code{TRUE}) rather than
#'   stopping with an error (\code{FALSE}, the default). Useful for blank/
#'   skipped responses that show up as e.g. \code{""} alongside real
#'   answers.
#'
#' @return \code{data} with the specified columns converted from text to
#'   numeric. The recoded column names are recorded as an attribute (
#'   \code{"rii_recoded_items"}), which \code{rii_table(items = "auto")}
#'   checks first — so recoding and then calling \code{rii_table(d,
#'   items = "auto")} finds exactly the columns you just recoded, even
#'   when their names are full question sentences that wouldn't otherwise
#'   match the short-code detection in \code{\link{detect_likert_items}}.
#'
#' @examples
#' d <- data.frame(
#'   Q1 = c("Strongly agree", "Agree", "Disagree", " strongly disagree "),
#'   Q2 = c("Neutral", "Strongly agree", "Agree", "Agree")
#' )
#' d2 <- recode_likert(d, cols = c("Q1", "Q2"), mapping = "agreement5")
#' d2
#' rii_table(d2, items = "auto")
#'
#' @export
recode_likert <- function(data, cols = NULL, mapping = "agreement5",
                           ignore_case = TRUE, trim = TRUE, allow_na = FALSE) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }

  if (is.null(cols)) {
    scan <- detect_text_likert_items(data)
    suggested <- scan[scan$Suggested, ]
    if (nrow(suggested) == 0) {
      stop("detect_text_likert_items() found no columns matching a known ",
           "Likert wording. Pass 'cols' and 'mapping' explicitly.",
           call. = FALSE)
    }
    out <- data
    for (i in seq_len(nrow(suggested))) {
      cn <- suggested$Column[i]
      dict <- .likert_dictionaries[[suggested$Matched_Scale[i]]]
      out[[cn]] <- .apply_likert_mapping(out[[cn]], dict, ignore_case, trim,
                                          allow_na, cn)
    }
    message(
      "recode_likert(): auto-detected and recoded ", nrow(suggested),
      " column(s):\n",
      paste0("  - ", suggested$Column, " (", suggested$Matched_Scale, ")",
             collapse = "\n")
    )
    attr(out, "rii_recoded_items") <- suggested$Column
    return(out)
  }

  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in 'data': ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  dict <- if (is.character(mapping) && length(mapping) == 1 &&
              mapping %in% names(.likert_dictionaries)) {
    .likert_dictionaries[[mapping]]
  } else if (!is.null(names(mapping)) && all(nzchar(names(mapping)))) {
    mapping
  } else {
    stop("'mapping' must be the name of a built-in scale (",
         paste(names(.likert_dictionaries), collapse = ", "),
         ") or a named numeric vector, e.g. c(\"no\" = 1, \"yes\" = 2).",
         call. = FALSE)
  }

  out <- data
  for (cn in cols) {
    out[[cn]] <- .apply_likert_mapping(out[[cn]], dict, ignore_case, trim,
                                        allow_na, cn)
  }
  mapping_label <- if (is.character(mapping) && length(mapping) == 1) mapping else "custom"
  message("recode_likert(): recoded ", length(cols), " column(s) using the '",
          mapping_label, "' mapping: ", paste(cols, collapse = ", "))
  attr(out, "rii_recoded_items") <- cols
  out
}

#' Detect columns of text Likert responses
#'
#' Scans every column of \code{data} and checks whether its values (after
#' trimming whitespace and ignoring case) are entirely covered by one of
#' the built-in Likert wordings used by \code{\link{recode_likert}} (e.g.
#' \code{"Strongly Disagree".."Strongly Agree"}). This is the text-data
#' equivalent of \code{\link{detect_likert_items}}, which works on
#' already-numeric columns.
#'
#' @param data A data frame.
#' @param dictionaries Named list of label-to-weight vectors to check
#'   against. Defaults to the built-in scales used by
#'   \code{\link{recode_likert}}; pass your own (in the same shape, e.g.
#'   \code{list(my_scale = c("no" = 1, "yes" = 2))}) to detect a wording
#'   not covered by the defaults.
#' @param max_unique Columns with more unique values than this are assumed
#'   to be free text (not a small fixed set of Likert labels) and are
#'   skipped without attempting to match. Defaults to 10.
#'
#' @return A data frame (class \code{"likert_text_scan"}, with a
#'   \code{print} method) with one row per column of \code{data}:
#'   \code{Column}, \code{N_Unique}, \code{Sample_Values},
#'   \code{Matched_Scale} (the name of the built-in scale it matched, or
#'   \code{NA}), \code{Suggested} (logical) and \code{Reason}.
#'
#' @examples
#' d <- data.frame(
#'   Comments = c("great app", "could be better", "no comment"),
#'   Q1 = c("Strongly agree", "Agree", "Disagree"),
#'   Q2 = c("Neutral", "Strongly agree", "Agree")
#' )
#' detect_text_likert_items(d)
#'
#' @export
detect_text_likert_items <- function(data, dictionaries = .likert_dictionaries,
                                      max_unique = 10) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }

  cols <- names(data)
  n <- length(cols)
  n_unique <- integer(n)
  sample_values <- character(n)
  matched_scale <- rep(NA_character_, n)
  suggested <- rep(FALSE, n)
  reason <- character(n)

  for (i in seq_len(n)) {
    x <- data[[cols[i]]]
    is_text <- is.character(x) || is.factor(x)
    x_chr <- if (is_text) as.character(x) else NA_character_
    uniq <- unique(trimws(x_chr[!is.na(x_chr) & nzchar(trimws(x_chr))]))
    n_unique[i] <- length(uniq)
    sample_values[i] <- paste(utils::head(uniq, 4), collapse = ", ")

    if (!is_text) {
      reason[i] <- "not a text column"
      next
    }
    if (length(uniq) == 0) {
      reason[i] <- "no non-missing text values"
      next
    }
    if (length(uniq) > max_unique) {
      reason[i] <- sprintf("%d unique values - likely free text, not Likert", length(uniq))
      next
    }

    uniq_l <- tolower(uniq)
    match_found <- NA_character_
    for (dict_name in names(dictionaries)) {
      dict_keys <- tolower(names(dictionaries[[dict_name]]))
      if (all(uniq_l %in% dict_keys)) {
        match_found <- dict_name
        break
      }
    }

    if (!is.na(match_found)) {
      matched_scale[i] <- match_found
      suggested[i] <- TRUE
      reason[i] <- sprintf("values match the '%s' scale", match_found)
    } else {
      reason[i] <- "values don't match a known scale wording"
    }
  }

  out <- data.frame(
    Column = cols, N_Unique = n_unique, Sample_Values = sample_values,
    Matched_Scale = matched_scale, Suggested = suggested, Reason = reason,
    stringsAsFactors = FALSE, row.names = NULL
  )
  class(out) <- c("likert_text_scan", "data.frame")
  out
}

#' Print a likert_text_scan
#'
#' @param x An object of class \code{"likert_text_scan"}.
#' @param ... Further arguments (currently unused).
#' @export
print.likert_text_scan <- function(x, ...) {
  n_sugg <- sum(x$Suggested)
  cat(sprintf("%d of %d columns match a known Likert wording\n", n_sugg, nrow(x)))
  cat(strrep("-", 40), "\n", sep = "")
  print.data.frame(as.data.frame(unclass(x)), row.names = FALSE)
  invisible(x)
}
