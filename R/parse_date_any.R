#' Parse a Date Vector Using Multiple Candidate Formats
#'
#' Converts a vector of dates (as character or Date-coercible values) into a
#' `data.table` `IDate` object, trying several common date formats in turn.
#'
#' @param x A vector to be converted to a date. Typically a character vector,
#'   but anything accepted by \code{\link[base]{as.Date}} will work.
#'
#' @details
#' This function is a thin wrapper around \code{as.Date(x, tryFormats = ...)}
#' followed by \code{data.table::as.IDate()}. The \code{tryFormats} argument
#' tells \code{as.Date} to attempt each format in this order, on the whole
#' vector at once:
#' \enumerate{
#'   \item \code{"\%d\%b\%Y"}  — e.g. "23Jul2026"
#'   \item \code{"\%Y-\%m-\%d"} — e.g. "2026-07-23"
#'   \item \code{"\%m/\%d/\%Y"} — e.g. "07/23/2026"
#'   \item \code{"\%d/\%m/\%Y"} — e.g. "23/07/2026"
#' }
#'
#' \strong{Important caveat:} \code{as.Date} does not try formats
#' element-by-element. It tries the first format against the *entire*
#' vector; if that format parses every non-\code{NA} element without
#' producing an \code{NA}, it stops and uses that format for all values.
#' Otherwise it moves to the next format, and so on. This means:
#' \itemize{
#'   \item If your vector mixes multiple date formats within the same
#'     column (e.g. some rows "\%d/\%m/\%Y", others "\%Y-\%m-\%d"), this
#'     function will \strong{not} correctly parse a mix — it applies one
#'     format to the whole vector.
#'   \item Ambiguous formats can silently produce wrong dates. For example
#'     "01/02/2026" is valid under both \code{"\%m/\%d/\%Y"} and
#'     \code{"\%d/\%m/\%Y"}; whichever format is tried first that succeeds
#'     for the whole vector wins, which may not be the format actually used
#'     by that particular value.
#'   \item If no format matches all elements, \code{as.Date} falls back to
#'     the last format tried, silently producing \code{NA} for elements
#'     that don't match it (with a warning).
#' }
#'
#' The result is coerced to \code{data.table::IDate}, an integer-backed date
#' class that's more memory-efficient than base \code{Date} and integrates
#' natively with \code{data.table} operations (joins, grouping, etc.).
#'
#' @return An object of class \code{IDate} (see \code{\link[data.table]{IDate}}),
#'   the same length as \code{x}.
#'
#' @examples
#' parse_date_any(c("23Jul2026", "23Jul2026"))
#' parse_date_any(c("2026-07-23", "2026-07-24"))
#' parse_date_any(c("07/23/2026", "07/24/2026"))
#'
#' @seealso \code{\link[base]{as.Date}}, \code{\link[data.table]{IDate}}
#' @export
parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}
