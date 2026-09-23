#' Plot a ranked RII bar chart
#'
#' Produces a horizontal bar chart of RII values, sorted from highest to
#' lowest, in the style typically used to report RII results in
#' environmental, construction and social science papers. Uses only base R
#' graphics, so it needs no extra packages. If the plotting window is
#' small, it automatically tries progressively more compact label sizes
#' and margins rather than failing outright with a "figure margins too
#' large" error, which is a common base-R issue with long item names in a
#' small plot pane.
#'
#' @param x An object of class \code{"rii_table"} (see
#'   \code{\link{rii_table}} or \code{\link{rii_table_freq}}).
#' @param top_n Optional integer; if given, only the top \code{top_n} items
#'   are plotted. Defaults to all items.
#' @param col Bar colour. Defaults to a green shade referencing the
#'   Nigerian flag, since the bundled example dataset is Nigerian; change
#'   freely for other contexts.
#' @param main Plot title. Defaults to \code{"Relative Importance Index"}.
#' @param show_values Logical; print the RII value at the end of each bar?
#'   Defaults to \code{TRUE}.
#' @param max_label_chars Maximum number of characters to show for each
#'   item label before truncating with "..."; also caps how wide the left
#'   margin can grow. Defaults to 28. This is only a starting point — if
#'   the plotting window is too small even for this, smaller values (and a
#'   smaller \code{cex.names}) are tried automatically before giving up.
#' @param cex.names Starting character expansion for the item labels,
#'   passed to \code{\link[graphics]{barplot}}. Also only a starting
#'   point when the window is small; see \code{max_label_chars}.
#' @param ... Further arguments passed on to \code{\link[graphics]{barplot}}.
#'
#' @return Invisibly, the bar midpoints returned by
#'   \code{\link[graphics]{barplot}}.
#'
#' @examples
#' data(building_collapse_nigeria)
#' tbl <- rii_table(building_collapse_nigeria[, 5:14])
#' plot(tbl)
#' plot(tbl, top_n = 5, col = "steelblue")
#'
#' @export
plot.rii_table <- function(x, top_n = NULL, col = "#008751",
                            main = "Relative Importance Index",
                            show_values = TRUE, max_label_chars = 28,
                            cex.names = 1, ...) {
  d <- x[order(x$Rank), ]
  if (!is.null(top_n)) d <- d[seq_len(min(top_n, nrow(d))), ]
  # Plot with the top-ranked item at the top of the chart
  d <- d[rev(seq_len(nrow(d))), ]

  make_labels <- function(chars) {
    ifelse(nchar(d$Item) > chars,
           paste0(substr(d$Item, 1, chars - 3), "..."),
           d$Item)
  }

  draw_once <- function(chars, mult, cex_n) {
    labels <- make_labels(chars)
    mar_left <- max(nchar(labels)) * mult + 4
    op <- graphics::par(mar = c(5, mar_left, 4, 3))
    on.exit(graphics::par(op))

    bp <- graphics::barplot(
      d$RII, names.arg = labels, horiz = TRUE, las = 1, col = col,
      border = NA, xlim = c(0, 1), xlab = "RII", main = main,
      cex.names = cex_n, ...
    )
    graphics::abline(v = seq(0, 1, by = 0.2), col = "grey85", lty = 3)
    graphics::barplot(d$RII, names.arg = labels, horiz = TRUE, las = 1,
                       col = col, border = NA, xlim = c(0, 1),
                       xlab = "RII", main = main, cex.names = cex_n,
                       add = TRUE)
    if (show_values) {
      graphics::text(d$RII + 0.02, bp, labels = sprintf("%.3f", d$RII),
                      adj = 0, cex = 0.8 * cex_n, xpd = NA)
    }
    bp
  }

  # Progressively more compact fallbacks for small plotting windows: each
  # step shortens labels and shrinks text further than the last.
  attempts <- list(
    c(chars = max_label_chars,               mult = 0.55, cex = cex.names),
    c(chars = min(max_label_chars, 20),       mult = 0.45, cex = cex.names * 0.85),
    c(chars = min(max_label_chars, 15),       mult = 0.38, cex = cex.names * 0.7),
    c(chars = min(max_label_chars, 10),       mult = 0.32, cex = cex.names * 0.6)
  )

  bp <- NULL
  last_error <- NULL
  for (a in attempts) {
    bp <- tryCatch(
      draw_once(a[["chars"]], a[["mult"]], a[["cex"]]),
      error = function(e) { last_error <<- e; NULL }
    )
    if (!is.null(bp)) break
  }

  if (is.null(bp)) {
    stop(
      "Could not draw the plot (", conditionMessage(last_error), "). ",
      "The plotting window is too small even after shrinking labels. ",
      "Try enlarging the plot window/device (e.g. RStudio's Plots pane), ",
      "or lower max_label_chars/cex.names further yourself, e.g. ",
      "plot(tbl, max_label_chars = 8, cex.names = 0.5).",
      call. = FALSE
    )
  }
  invisible(bp)
}
