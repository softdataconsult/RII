#' Build a ranked RII table from raw item-level responses
#'
#' This is the main entry point most users will call. Give it a data frame
#' (or matrix) of raw Likert responses — one row per respondent, one column
#' per item/factor — and it returns a fully ranked table with N, Mean, SD,
#' RII and importance band for every item, sorted from most to least
#' important. Works with any Likert scale range, and can reverse-score
#' negatively-worded items before ranking.
#'
#' @param data A data frame or matrix of raw Likert scores. Each column is
#'   one item being ranked; each row is one respondent. Non-numeric columns
#'   (e.g. respondent ID, profession) should not be included — use
#'   \code{items} to select just the relevant columns if needed.
#' @param items Optional character vector of column names, or numeric
#'   vector of column positions (e.g. \code{11:17} for a single section/
#'   construct out of a longer questionnaire), in \code{data} to treat as
#'   the items to rank. Defaults to all columns of \code{data}.
#'   Pass \code{"auto"} to have \code{\link{detect_likert_items}} pick the
#'   item columns automatically from a raw survey export — useful when
#'   \code{data} has many demographic/ID columns mixed in with the Likert
#'   items (see \code{\link{detect_likert_items}} for how this works, and
#'   always review the message it prints before trusting the result).
#' @param scale_max The highest point on the Likert scale used. Defaults to
#'   5.
#' @param scale_min The lowest point on the Likert scale used. Defaults to
#'   1. Set to \code{0} for a 0-based scale, or adjust for any other range
#'   (a 3-point, 7-point, etc. scale).
#' @param reverse_items Optional character vector of item names (a subset
#'   of \code{items}/\code{names(data)}) to reverse-score before computing
#'   RII — for items that were worded negatively relative to the rest of
#'   the instrument. See \code{\link{reverse_score}}.
#' @param na.rm Logical; drop missing responses item by item when computing
#'   N, Mean, SD and RII? Defaults to \code{TRUE}.
#'
#' @return An object of class \code{"rii_table"}, which is a data frame
#'   with columns \code{Item}, \code{N}, \code{Mean}, \code{SD}, \code{RII},
#'   \code{Rank} and \code{Band}, sorted by \code{RII} in descending order.
#'   It has \code{print}, \code{summary} and \code{plot} methods.
#'
#' @examples
#' data(building_collapse_nigeria)
#' items <- building_collapse_nigeria[, 5:14]
#' tbl <- rii_table(items)
#' tbl
#' plot(tbl)
#'
#' # A 0-based scale, with one negatively-worded item reverse-scored
#' d0 <- items - 1                                  # simulate a 0-4 scale
#' rii_table(d0, scale_min = 0, scale_max = 4,
#'           reverse_items = "Overloading")
#'
#' # A raw survey export with demographic columns mixed in with items
#' # named in the common short-code convention (OJ1, PJ6, ...): items = "auto"
#' # picks out just the Likert-item columns
#' set.seed(1)
#' raw <- data.frame(
#'   `S/N` = 1:30, GENDER = sample(1:2, 30, TRUE),
#'   OJ1 = sample(1:5, 30, TRUE), OJ2 = sample(1:5, 30, TRUE),
#'   PJ6 = sample(1:5, 30, TRUE), check.names = FALSE
#' )
#' rii_table(raw, items = "auto")
#'
#' # Selecting one section/construct out of a longer questionnaire by
#' # column position — e.g. columns 11:17 hold the "Road Surface Quality"
#' # section, out of a much wider file
#' # rii_table(full_survey_data, items = 11:17)
#'
#' @export
rii_table <- function(data, items = NULL, scale_max = 5, scale_min = 1,
                       reverse_items = NULL, na.rm = TRUE) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix of raw Likert scores.",
         call. = FALSE)
  }
  data <- as.data.frame(data)
  if (is.numeric(items)) {
    if (any(items < 1 | items > ncol(data) | items != as.integer(items))) {
      stop("'items', when numeric, must be whole-number column positions ",
           "between 1 and ncol(data) (", ncol(data), ").", call. = FALSE)
    }
    items <- names(data)[items]
  }
  if (identical(items, "auto")) {
    recoded <- attr(data, "rii_recoded_items")
    if (!is.null(recoded) && length(recoded) > 0) {
      items <- recoded
      message(
        "rii_table(items = \"auto\"): using the ", length(items), " column(s) ",
        "already recoded by recode_likert(): ", paste(items, collapse = ", ")
      )
    } else {
      scan <- detect_likert_items(data)
      items <- scan$Column[scan$Suggested]
      excluded <- scan$Column[!scan$Suggested]
      if (length(items) == 0) {
        stop("detect_likert_items() found no likely item columns in 'data'. ",
             "Run detect_likert_items(data) yourself and pass 'items' ",
             "explicitly.", call. = FALSE)
      }
      message(
        "rii_table(items = \"auto\"): using ", length(items), " detected item ",
        "column(s): ", paste(items, collapse = ", "), "\n",
        "Excluded ", length(excluded), " column(s): ",
        paste(excluded, collapse = ", "), "\n",
        "Run detect_likert_items(data) yourself to review or adjust this."
      )
    }
  }
  if (!is.null(items)) {
    missing_cols <- setdiff(items, names(data))
    if (length(missing_cols) > 0) {
      stop("Column(s) not found in 'data': ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    data <- data[, items, drop = FALSE]
  }

  non_numeric <- names(data)[!vapply(data, is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("Non-numeric column(s) found: ", paste(non_numeric, collapse = ", "),
         ". Pass only the Likert-scored item columns (use 'items' to select ",
         "them), and convert factors to numeric scores first.", call. = FALSE)
  }

  if (!is.null(reverse_items)) {
    bad <- setdiff(reverse_items, names(data))
    if (length(bad) > 0) {
      stop("'reverse_items' not found among the item columns: ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
  }

  res <- Map(
    function(col, nm) {
      rii_raw(col, scale_max = scale_max, scale_min = scale_min,
              reverse = nm %in% reverse_items, na.rm = na.rm)
    },
    data, names(data)
  )
  out <- do.call(rbind, res)
  tbl <- data.frame(
    Item = names(data),
    N    = out[, "n"],
    Mean = round(out[, "mean"], 3),
    SD   = round(out[, "sd"], 3),
    RII  = round(out[, "rii"], 3),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  tbl <- tbl[order(-tbl$RII), ]
  tbl$Rank <- seq_len(nrow(tbl))
  tbl$Band <- as.character(classify_rii(tbl$RII))
  rownames(tbl) <- NULL

  class(tbl) <- c("rii_table", "data.frame")
  attr(tbl, "scale_max") <- scale_max
  attr(tbl, "scale_min") <- scale_min
  tbl
}

#' Build a ranked RII table from a frequency table
#'
#' Use this instead of \code{\link{rii_table}} when, for each item, you
#' only have the count of respondents choosing each scale point (the
#' typical layout of a spreadsheet RII template), rather than raw
#' respondent-level data. Any set of weights is supported (a 0-based
#' scale, a 3- or 7-point scale, etc.), and negatively-worded items can be
#' reverse-scored before ranking.
#'
#' @param freq_data A data frame or matrix with one row per item and one
#'   column per scale point (in increasing order of weight), containing
#'   respondent counts. Row names (or an \code{item} column, see
#'   \code{item_col}) are used as item labels.
#' @param weights The weight associated with each column of
#'   \code{freq_data}. Defaults to \code{1:ncol(freq_data)} once
#'   \code{item_col} (if any) has been removed; pass e.g.
#'   \code{0:(ncol(freq_data) - 1)} for a 0-based scale.
#' @param item_col Optional name (or index) of a column in \code{freq_data}
#'   holding item labels, if labels are not already in the row names.
#' @param reverse_items Optional character vector of item labels (a subset
#'   of the resolved item names) to reverse-score before computing RII —
#'   for items that were worded negatively relative to the rest of the
#'   instrument. Assumes \code{weights} is an evenly-spaced increasing
#'   sequence.
#'
#' @return An object of class \code{"rii_table"}; see \code{\link{rii_table}}.
#'
#' @examples
#' freq <- data.frame(
#'   Item = c("Poor supervision", "Use of substandard materials"),
#'   n1 = c(2, 1), n2 = c(5, 4), n3 = c(10, 12), n4 = c(38, 40), n5 = c(95, 93)
#' )
#' rii_table_freq(freq, item_col = "Item")
#'
#' @export
rii_table_freq <- function(freq_data, weights = NULL, item_col = NULL,
                            reverse_items = NULL) {
  if (!is.data.frame(freq_data) && !is.matrix(freq_data)) {
    stop("'freq_data' must be a data frame or matrix.", call. = FALSE)
  }
  freq_data <- as.data.frame(freq_data)

  if (!is.null(item_col)) {
    item_names <- as.character(freq_data[[item_col]])
    freq_data[[item_col]] <- NULL
  } else if (!is.null(rownames(freq_data)) &&
             !identical(rownames(freq_data), as.character(seq_len(nrow(freq_data))))) {
    item_names <- rownames(freq_data)
  } else {
    item_names <- paste0("Item", seq_len(nrow(freq_data)))
  }

  non_numeric <- names(freq_data)[!vapply(freq_data, is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("Non-numeric column(s) found in 'freq_data': ",
         paste(non_numeric, collapse = ", "), call. = FALSE)
  }

  if (!is.null(reverse_items)) {
    bad <- setdiff(reverse_items, item_names)
    if (length(bad) > 0) {
      stop("'reverse_items' not found among the item labels: ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
  }

  if (is.null(weights)) weights <- seq_len(ncol(freq_data))
  scale_max <- max(weights)
  scale_min <- min(weights)

  res <- Map(
    function(i, nm) {
      rii_freq(as.numeric(freq_data[i, ]), weights = weights,
               reverse = nm %in% reverse_items)
    },
    seq_len(nrow(freq_data)), item_names
  )
  res <- do.call(rbind, res)

  tbl <- data.frame(
    Item = item_names,
    N    = res[, "n"],
    Mean = round(res[, "mean"], 3),
    SD   = round(res[, "sd"], 3),
    RII  = round(res[, "rii"], 3),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  tbl <- tbl[order(-tbl$RII), ]
  tbl$Rank <- seq_len(nrow(tbl))
  tbl$Band <- as.character(classify_rii(tbl$RII))
  rownames(tbl) <- NULL

  class(tbl) <- c("rii_table", "data.frame")
  attr(tbl, "scale_max") <- scale_max
  attr(tbl, "scale_min") <- scale_min
  tbl
}
