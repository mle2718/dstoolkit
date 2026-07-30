#' Validate data for Dashboard
#'
#' Validation function that ensure that the data we have is the
#' data we want to push to the dashboard
#'
#' @param df long dataframe with numbers at age.
#' @param age_classes number of age classes.
#' @param ndraws=1 number of random draws in the dataframe
#' @param years=1 number of years in the dataframe

#' @details
#'  df is expcted to contain character variables for
#'     fishery, common name, metric, source,  units.
#'  df is expected to contain numerics for
#'   itis code (species_itis) and value.
#'  df is expectd to contain a data_version, which is a Date
#'  df is expected to contain state and wave. These may be NA
#'  ndraws and years are used with age_classes to ensure the proper number of rows
#'  #' @return nothing
#'
#'   @export


########################################################
# Define the validation function
# Is our data what it claims to be.  We should have some characters, some
# numerics, a date. These should have no missing values.
########################################################
validate_naa_data <- function(df, age_classes, ndraws=1, years=1) {

  # Ensure specified columns are character vectors and contain no NAs
  stopifnot(
    is.character(df$fishery) && !any(is.na(df$fishery)),
    is.character(df$common) && !any(is.na(df$common)),
    is.character(df$stock_abbrev) && !any(is.na(df$stock_abbrev)),
    is.character(df$metric) && !any(is.na(df$metric)),
    is.character(df$source) && !any(is.na(df$source)),
    is.character(df$units) && !any(is.na(df$units))
  )

  # Ensure species_itis and value are numeric
  stopifnot(is.numeric(df$species_itis))
  stopifnot(is.numeric(df$value))

  # Ensure data_version is a Date class
  stopifnot(inherits(df$data_version, "Date"))

  # Historical data should have the same number of rows as age classes* years.
  # projected data  should have age_classes*ndraws*years
    stopifnot(nrow(df) == age_classes * ndraws*years)



  # NOTE: state and wave are allowed to be NA; no type enforcement applied here

  # Return the dataframe invisibly to support tidyverse piping (%>%)
  invisible(df)
}
