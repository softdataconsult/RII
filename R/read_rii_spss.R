#' Read Likert/RII data straight from an SPSS (.sav) file
#'
#' The SPSS equivalent of \code{\link{read_rii_excel}} — same two layouts
#' (\code{"raw"} and \code{"freq"}, see there for what each means), same
#' \code{items = "auto"} detection support, but for a \code{.sav} file.
#' Uses the \pkg{haven} package to read the file, and converts SPSS's
#' "labelled" values (e.g. \code{1} with the label \code{"Strongly
#' Disagree"} attached) to plain numeric scores before computing RII,
#' since the label text itself isn't needed for the calculation.
#'
#' @param path Path to the \code{.sav} file.
#' @param layout Either \code{"raw"} or \code{"freq"}; see
#'   \code{\link{read_rii_excel}} for what each means.
#' @param items For \code{layout = "raw"}: character vector of column
#'   names, or numeric vector of column positions (e.g. \code{11:17} for
#'   one section/construct of a longer questionnaire), to treat as the
#'   items to rank. Defaults to all numeric columns in the file. Pass
#'   \code{"auto"} to have \code{\link{detect_likert_items}} pick them
#'   for you.
#' @param item_col For \code{layout = "freq"}: name of the column holding
#'   item labels (defaults to the first non-numeric column found).
#' @param scale_max For \code{layout = "raw"}, the highest Likert point.
#'   Defaults to 5.
#' @param scale_min For \code{layout = "raw"}, the lowest Likert point.
#'   Defaults to 1; set to \code{0} for a 0-based scale, or adjust for any
#'   other range.
#' @param reverse_items Optional character vector of item names/labels to
#'   reverse-score before computing RII. See \code{\link{reverse_score}}.
#' @param weights For \code{layout = "freq"}, the weight for each scale
#'   column, in the order the columns appear. Defaults to
#'   \code{1:number of scale columns}.
#' @param recode_text Logical; if \code{TRUE}, run \code{\link{recode_likert}}
#'   on the data right after reading it — auto-detecting and converting any
#'   text Likert columns to numeric — before computing RII. Only applies
#'   when \code{layout = "raw"}. Rarely needed for SPSS files, since SPSS
#'   usually stores Likert responses as labelled numeric codes (already
#'   handled automatically) rather than plain text; defaults to
#'   \code{FALSE}.
#'
#' @return An object of class \code{"rii_table"}; see
#'   \code{\link{rii_table}}.
#'
#' @examples
#' \dontrun{
#' read_rii_spss("survey.sav", layout = "raw", items = "auto")
#' }
#'
#' @export
read_rii_spss <- function(path, layout = c("raw", "freq"), items = NULL,
                           item_col = NULL, scale_max = 5, scale_min = 1,
                           reverse_items = NULL, weights = NULL,
                           recode_text = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Reading SPSS files requires the 'haven' package. ",
         "Install it with install.packages(\"haven\").", call. = FALSE)
  }
  layout <- match.arg(layout)
  d <- haven::read_sav(path)
  # Strip SPSS value labels so numeric/is.numeric() checks downstream
  # (item detection, RII calculation) see plain numeric/character data
  # rather than haven_labelled columns.
  d <- as.data.frame(lapply(d, function(col) {
    if (inherits(col, "haven_labelled")) {
      col <- haven::zap_labels(col)
    }
    if (inherits(col, "haven_labelled_spss")) {
      col <- haven::zap_labels(col)
    }
    as.vector(col)
  }), stringsAsFactors = FALSE)
  names(d) <- make.names(names(d), unique = TRUE)

  .rii_from_dataframe(d, layout, items, item_col, scale_max, scale_min,
                       reverse_items, weights, recode_text = recode_text)
}
