#' Reverse-score Likert responses
#'
#' Flips raw Likert scores around the midpoint of the scale, for items that
#' were worded negatively relative to the rest of the instrument (a common
#' need before computing RII, since mixing normally- and negatively-worded
#' items without reversing one of them will distort the ranking).
#'
#' @param x A numeric vector of raw Likert scores.
#' @param scale_min The lowest point on the scale (e.g. 1, or 0 for a
#'   0-based scale). Defaults to 1.
#' @param scale_max The highest point on the scale (e.g. 5, or 4 for a
#'   0-based 5-point scale; 7 for a 7-point scale; and so on). Defaults to
#'   5.
#'
#' @return A numeric vector the same length as \code{x}, with each score
#'   \code{v} replaced by \code{scale_min + scale_max - v}.
#'
#' @examples
#' reverse_score(c(1, 2, 3, 4, 5))            # -> 5 4 3 2 1
#' reverse_score(c(0, 1, 2, 3, 4), scale_min = 0, scale_max = 4)  # -> 4 3 2 1 0
#'
#' @export
reverse_score <- function(x, scale_min = 1, scale_max = 5) {
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.", call. = FALSE)
  }
  if (scale_min >= scale_max) {
    stop("'scale_min' must be less than 'scale_max'.", call. = FALSE)
  }
  scale_min + scale_max - x
}

#' Relative Importance Index from raw Likert scores
#'
#' Computes the Relative Importance Index (RII), mean, standard deviation
#' and respondent count for a single item, given the raw (respondent-level)
#' Likert scores for that item. Works with any scale range — the standard
#' 1-5 scale, a 0-based scale (e.g. 0-4), a 3- or 7-point scale, and so on
#' — and can reverse-score negatively-worded items first.
#'
#' The Relative Importance Index is defined as
#' \deqn{RII = \frac{\sum W}{A \times N}}
#' where \eqn{\sum W} is the sum of the weights given by all respondents
#' (i.e. the sum of their raw scores), \eqn{A} is the highest possible
#' weight on the scale (e.g. 5 for a 5-point scale, whatever the lowest
#' point is), and \eqn{N} is the number of respondents who rated the item.
#'
#' @param x A numeric vector of raw Likert scores, one value per
#'   respondent. Values outside \code{scale_min:scale_max} are not
#'   allowed.
#' @param scale_max The highest point on the Likert scale used (the "A" in
#'   the RII formula). Defaults to 5, the most common case.
#' @param scale_min The lowest point on the Likert scale used. Defaults to
#'   1. Set to \code{0} for a 0-based scale (e.g. 0-4), or adjust for any
#'   other range (e.g. a 7-point 1-7 scale, a 3-point 1-3 scale).
#' @param reverse Logical; reverse-score \code{x} (see
#'   \code{\link{reverse_score}}) before computing, for a negatively-worded
#'   item. Defaults to \code{FALSE}.
#' @param na.rm Logical; should missing responses be dropped before
#'   computing N, the mean and the RII? Defaults to \code{TRUE}.
#'
#' @return A named numeric vector with elements \code{n}, \code{sum},
#'   \code{mean}, \code{sd} and \code{rii}.
#'
#' @examples
#' # 12 respondents rate one factor on a 5-point (1-5) scale
#' scores <- c(5, 4, 5, 3, 5, 4, 4, 5, 2, 5, 4, 5)
#' rii_raw(scores)
#'
#' # A 0-based 5-point scale (0-4)
#' rii_raw(c(4, 3, 4, 2, 4), scale_min = 0, scale_max = 4)
#'
#' # A negatively-worded item on a 1-5 scale, reverse-scored before ranking
#' rii_raw(c(1, 2, 1, 2, 1), reverse = TRUE)
#'
#' @export
rii_raw <- function(x, scale_max = 5, scale_min = 1, reverse = FALSE,
                     na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector of raw Likert scores.", call. = FALSE)
  }
  if (scale_min >= scale_max) {
    stop("'scale_min' must be less than 'scale_max'.", call. = FALSE)
  }
  if (na.rm) x <- x[!is.na(x)]
  if (any(x < scale_min | x > scale_max, na.rm = TRUE)) {
    stop("All scores must lie between scale_min (", scale_min,
         ") and scale_max (", scale_max, ").", call. = FALSE)
  }
  if (reverse) x <- reverse_score(x, scale_min = scale_min, scale_max = scale_max)
  n <- length(x)
  if (n == 0) {
    return(c(n = 0, sum = NA_real_, mean = NA_real_, sd = NA_real_,
             rii = NA_real_))
  }
  s <- sum(x)
  c(
    n    = n,
    sum  = s,
    mean = s / n,
    sd   = if (n > 1) stats::sd(x) else NA_real_,
    rii  = s / (scale_max * n)
  )
}

#' Relative Importance Index from a frequency table
#'
#' Many researchers (and most spreadsheet RII templates) do not keep raw
#' respondent-level data — they only tally how many respondents chose each
#' point on the Likert scale for a given item. \code{rii_freq()} computes
#' the RII, mean and standard deviation directly from such a frequency
#' count, without needing the raw data. Any scale (any set of weights, not
#' just 1..A) is supported via \code{weights}.
#'
#' @param freq A numeric vector of counts, one per scale point, in
#'   increasing order of weight (e.g. \code{c(n1, n2, n3, n4, n5)} for a
#'   5-point scale, where \code{n1} is the number of respondents who chose
#'   the lowest point).
#' @param weights The weight associated with each element of \code{freq},
#'   in the same order. Defaults to \code{1:length(freq)}, i.e. a standard
#'   1..A Likert scale; pass e.g. \code{0:(length(freq) - 1)} for a
#'   0-based scale, or any other set of weights your instrument uses.
#' @param reverse Logical; reverse the correspondence between \code{freq}
#'   and \code{weights} (equivalent to reverse-scoring every respondent),
#'   for a negatively-worded item. Defaults to \code{FALSE}. Assumes
#'   \code{weights} is an evenly-spaced increasing sequence (true for
#'   essentially all Likert scales).
#'
#' @return A named numeric vector with elements \code{n}, \code{sum},
#'   \code{mean}, \code{sd} and \code{rii}, in the same units as
#'   \code{\link{rii_raw}}.
#'
#' @examples
#' # 5-point scale: 1 respondent chose 1, 2 chose 2, 10 chose 3, 25 chose 4,
#' # 42 chose 5
#' rii_freq(c(1, 2, 10, 25, 42))
#'
#' # A 0-based 5-point scale (0-4)
#' rii_freq(c(1, 2, 10, 25, 42), weights = 0:4)
#'
#' # A negatively-worded item, reverse-scored before ranking
#' rii_freq(c(1, 2, 10, 25, 42), reverse = TRUE)
#'
#' @export
rii_freq <- function(freq, weights = seq_along(freq), reverse = FALSE) {
  if (!is.numeric(freq) || !is.numeric(weights)) {
    stop("'freq' and 'weights' must be numeric.", call. = FALSE)
  }
  if (length(freq) != length(weights)) {
    stop("'freq' and 'weights' must be the same length.", call. = FALSE)
  }
  if (any(freq < 0, na.rm = TRUE)) {
    stop("'freq' cannot contain negative counts.", call. = FALSE)
  }
  if (reverse) freq <- rev(freq)
  n <- sum(freq)
  a <- max(weights)
  if (n == 0) {
    return(c(n = 0, sum = NA_real_, mean = NA_real_, sd = NA_real_,
             rii = NA_real_))
  }
  s <- sum(freq * weights)
  m <- s / n
  # Variance from grouped/frequency data
  var_g <- sum(freq * (weights - m)^2) / (n - 1)
  c(
    n    = n,
    sum  = s,
    mean = m,
    sd   = if (n > 1) sqrt(var_g) else NA_real_,
    rii  = s / (a * n)
  )
}

#' Classify RII values into importance bands
#'
#' Assigns each RII value to one of the five importance bands commonly
#' used in the RII literature (e.g. construction and environmental
#' management studies), so results can be described qualitatively as well
#' as numerically. This works the same way regardless of the original
#' scale range, since RII itself is always normalized to 0-1.
#'
#' @param rii A numeric vector of RII values (each between 0 and 1).
#'
#' @return A factor, the same length as \code{rii}, with levels
#'   \code{"Low"}, \code{"Medium-Low"}, \code{"Medium"},
#'   \code{"High-Medium"} and \code{"High"}.
#'
#' @examples
#' classify_rii(c(0.92, 0.71, 0.55, 0.33, 0.12))
#'
#' @export
classify_rii <- function(rii) {
  if (!is.numeric(rii) || any(rii < 0 | rii > 1, na.rm = TRUE)) {
    stop("'rii' must be numeric with values between 0 and 1.", call. = FALSE)
  }
  breaks <- c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf)
  labels <- c("Low", "Medium-Low", "Medium", "High-Medium", "High")
  cut(rii, breaks = breaks, labels = labels, right = TRUE)
}
