#' Write RII results to an Excel workbook
#'
#' Saves a ranked \code{\link{rii_table}} (or \code{\link{rii_table_freq}})
#' result to a formatted \code{.xlsx} workbook, so results can be shared
#' with co-authors, pasted into a report, or handed to someone who
#' isn't using R — the mirror image of \code{\link{read_rii_excel}}. The
#' table is written as a real Excel table (with filter arrows), the
#' importance \code{Band} column is colour-coded the way many published
#' RII tables shade it, and by default the ranked bar chart is embedded
#' in the same sheet.
#'
#' @param tbl An object of class \code{"rii_table"} (from
#'   \code{\link{rii_table}}, \code{\link{rii_table_freq}} or
#'   \code{\link{read_rii_excel}}).
#' @param path Path to write the \code{.xlsx} file to.
#' @param sheet Worksheet name. Defaults to \code{"RII Results"}.
#' @param include_chart Logical; embed the ranked bar chart (see
#'   \code{\link{plot.rii_table}}) in the sheet next to the table?
#'   Defaults to \code{TRUE}. Requires the \pkg{openxlsx} image-insertion
#'   backend, which needs a working graphics device (this is normally the
#'   case; on a headless server without any graphics device at all, set
#'   this to \code{FALSE}).
#' @param overwrite Logical; overwrite \code{path} if it already exists?
#'   Defaults to \code{TRUE}.
#' @param ... Further arguments passed to \code{\link{plot.rii_table}}
#'   when \code{include_chart = TRUE} (e.g. \code{col}, \code{top_n}).
#'
#' @return Invisibly, \code{path}.
#'
#' @examples
#' data(building_collapse_nigeria)
#' tbl <- rii_table(building_collapse_nigeria[, 5:14])
#' out <- tempfile(fileext = ".xlsx")
#' write_rii_excel(tbl, out)
#' file.exists(out)
#' unlink(out)
#'
#' @export
write_rii_excel <- function(tbl, path, sheet = "RII Results",
                             include_chart = TRUE, overwrite = TRUE, ...) {
  if (!inherits(tbl, "rii_table")) {
    stop("'tbl' must be an rii_table object (from rii_table(), ",
         "rii_table_freq(), or read_rii_excel()).", call. = FALSE)
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Writing Excel files requires the 'openxlsx' package. ",
         "Install it with install.packages(\"openxlsx\").", call. = FALSE)
  }

  df <- as.data.frame(unclass(tbl))
  scale_max <- attr(tbl, "scale_max")
  scale_min <- attr(tbl, "scale_min")
  if (is.null(scale_min)) scale_min <- 1

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)

  title <- if (!is.null(scale_max)) {
    sprintf("Relative Importance Index Results (scale %s-%s)",
            scale_min, scale_max)
  } else {
    "Relative Importance Index Results"
  }
  openxlsx::writeData(wb, sheet, title, startRow = 1, startCol = 1)
  openxlsx::addStyle(
    wb, sheet,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 14),
    rows = 1, cols = 1
  )

  start_row <- 3
  openxlsx::writeDataTable(
    wb, sheet, df, startRow = start_row, tableStyle = "TableStyleMedium2",
    withFilter = TRUE
  )

  # Colour-code the importance band, the way many published RII tables
  # shade their results (red = Low ... green = High).
  band_colors <- c(
    "Low"         = "#F8696B",
    "Medium-Low"  = "#FFC7A0",
    "Medium"      = "#FFEB84",
    "High-Medium" = "#C6E0B4",
    "High"        = "#63BE7B"
  )
  band_col_idx <- which(names(df) == "Band")
  if (length(band_col_idx) == 1) {
    for (band in names(band_colors)) {
      row_idx <- which(df$Band == band) + start_row  # +header row
      if (length(row_idx) > 0) {
        openxlsx::addStyle(
          wb, sheet,
          style = openxlsx::createStyle(fgFill = band_colors[[band]]),
          rows = row_idx, cols = band_col_idx, gridExpand = TRUE, stack = TRUE
        )
      }
    }
  }

  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(df)), widths = "auto")
  openxlsx::freezePane(wb, sheet, firstActiveRow = start_row + 1)

  if (include_chart) {
    tmp_png <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_png), add = TRUE)
    grDevices::png(tmp_png, width = 1000, height = 700, res = 150)
    plot_ok <- tryCatch({
      plot(tbl, ...)
      TRUE
    }, error = function(e) FALSE)
    grDevices::dev.off()
    if (plot_ok) {
      openxlsx::insertImage(
        wb, sheet, tmp_png,
        startRow = start_row, startCol = ncol(df) + 2,
        width = 6, height = 4.2, units = "in"
      )
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = overwrite)
  invisible(path)
}
