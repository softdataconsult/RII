#' Read Likert/RII data straight from a CSV file
#'
#' The CSV equivalent of \code{\link{read_rii_excel}} — same two layouts
#' (\code{"raw"} and \code{"freq"}, see there for what each means), same
#' \code{items = "auto"} detection support, but for a \code{.csv} file.
#' Uses only base R (\code{\link[utils]{read.csv}}), so no extra packages
#' are required.
#'
#' @param path Path to the \code{.csv} file.
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
#' @param sep Field separator, passed to \code{\link[utils]{read.csv}}.
#'   Defaults to \code{","}; use \code{";"} for semicolon-separated files
#'   (common when the file was exported from a European-locale Excel), or
#'   \code{"\\t"} for tab-separated.
#' @param recode_text Logical; if \code{TRUE}, run \code{\link{recode_likert}}
#'   on the data right after reading it — auto-detecting and converting any
#'   text Likert columns ("Strongly Agree", "Disagree", ...) to numeric —
#'   before computing RII. Only applies when \code{layout = "raw"}.
#'   Defaults to \code{FALSE}, i.e. assumes the file already has numeric
#'   scores. Combine with \code{items = "auto"} for a one-line read of a
#'   raw text-Likert survey export.
#' @param ... Further arguments passed to \code{\link[utils]{read.csv}}
#'   (e.g. \code{encoding}, \code{na.strings}).
#'
#' @return An object of class \code{"rii_table"}; see
#'   \code{\link{rii_table}}.
#'
#' @examples
#' data(building_collapse_nigeria)
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(building_collapse_nigeria, tmp, row.names = FALSE)
#' read_rii_csv(tmp, layout = "raw",
#'              items = c("SubstandardMaterials", "PoorSupervision"))
#' unlink(tmp)
#'
#' # A file with text Likert labels ("Strongly Agree", etc.) instead of
#' # numbers: recode_text = TRUE handles the conversion in the same call
#' tmp2 <- tempfile(fileext = ".csv")
#' write.csv(data.frame(
#'   Gender = c("Male", "Female", "Male"),
#'   Q1 = c("Strongly agree", "Agree", "Disagree")
#' ), tmp2, row.names = FALSE)
#' read_rii_csv(tmp2, layout = "raw", items = "auto", recode_text = TRUE)
#' unlink(tmp2)
#'
#' @export
read_rii_csv <- function(path, layout = c("raw", "freq"), items = NULL,
                          item_col = NULL, scale_max = 5, scale_min = 1,
                          reverse_items = NULL, weights = NULL, sep = ",",
                          recode_text = FALSE, ...) {
  layout <- match.arg(layout)
  d <- utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, ...)

  .rii_from_dataframe(d, layout, items, item_col, scale_max, scale_min,
                       reverse_items, weights, recode_text = recode_text)
}
