#' Print an rii_table
#'
#' @param x An object of class \code{"rii_table"}.
#' @param ... Further arguments (currently unused).
#' @export
print.rii_table <- function(x, ...) {
  scale_max <- attr(x, "scale_max")
  scale_min <- attr(x, "scale_min")
  if (is.null(scale_min)) scale_min <- 1
  cat("Relative Importance Index table",
      if (!is.null(scale_max)) paste0(" (scale ", scale_min, "-", scale_max, ")") else "",
      "\n", sep = "")
  cat(strrep("-", 40), "\n", sep = "")
  print.data.frame(as.data.frame(unclass(x)), row.names = FALSE)
  invisible(x)
}

#' Summarize an rii_table
#'
#' Reports the top-ranked item, the spread of RII values, and how many
#' items fall into each importance band.
#'
#' @param object An object of class \code{"rii_table"}.
#' @param ... Further arguments (currently unused).
#' @export
summary.rii_table <- function(object, ...) {
  top <- object[object$Rank == 1, ]
  cat("Items ranked          :", nrow(object), "\n")
  cat("Top-ranked item        :", top$Item, sprintf("(RII = %.3f)", top$RII), "\n")
  cat("RII range              :", sprintf("%.3f to %.3f", min(object$RII), max(object$RII)), "\n")
  cat("Mean RII across items  :", sprintf("%.3f", mean(object$RII)), "\n\n")
  cat("Items per importance band:\n")
  print(table(factor(object$Band,
                      levels = c("High", "High-Medium", "Medium",
                                 "Medium-Low", "Low"))))
  invisible(object)
}
