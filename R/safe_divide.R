#' Safely Divide Two Numeric Vectors
#'
#' Performs element-wise division while guarding against division by zero
#' and missing denominators, returning \code{NA} instead of \code{Inf},
#' \code{-Inf}, or \code{NaN}.
#'
#' @param num A numeric vector (the numerator).
#' @param den A numeric vector (the denominator), the same length as
#'   \code{num} (or recyclable to it).
#'
#' @details
#' For each element, this function checks whether the denominator is
#' \code{NA} or equal to \code{0}. If so, it returns \code{NA_real_} for
#' that element; otherwise it returns \code{num / den} as usual.
#'
#' This avoids the base R behavior of division by zero silently producing
#' \code{Inf}, \code{-Inf}, or \code{NaN} (e.g. \code{1/0}, \code{-1/0},
#' \code{0/0}), which can otherwise propagate unnoticed through downstream
#' calculations, aggregations, or joins.
#'
#' Vectorization is handled via \code{\link[base]{ifelse}}, so \code{num}
#' and \code{den} should be the same length or safely recyclable, following
#' standard R recycling rules.
#'
#' @return A numeric (double) vector the same length as \code{num}/\code{den},
#'   with \code{NA_real_} wherever \code{den} is \code{NA} or \code{0}, and
#'   \code{num / den} otherwise.
#'
#' @examples
#' safe_divide(c(10, 5, 8), c(2, 0, NA))
#' #> [1]  5 NA NA
#'
#' safe_divide(4, 0)
#' #> [1] NA
#'
#' @seealso \code{\link[base]{ifelse}}
#' @export
safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}
