#' Reshape Numbers at Age to Long
#'
#' Data dashboard helper file that reshapes NAA from Wide to Long format
#'
#' @param df dataframe with columns containing numbers at age, those columns
#' start with "age"
#'
#' @details
#' A simple grep and pivot longer
#'
#' @return A long dataframe
#'
#' @examples
#' sample_df <- tibble(
#'   year   = c(2001:2002),
#'   metric = rep(c("NAA"), times=2),
#'   age2   = c(500, 150),
#'   age3   = c(250, 100))
#' > pivot_naa_long(sample_df)
#'  # A tibble: 4 × 3
#'  year metric value
#'  <int> <glue> <dbl>
#'  1  2001 NAA 2    500
#'  2  2001 NAA 3    250
#'  3  2002 NAA 2    150
#'  4  2002 NAA 3    100#'
#' @export
pivot_naa_long <- function(df) {
  age_cols <- grep("^age\\d+$", names(df), value = TRUE)
  df %>%
    tidyr::pivot_longer(cols = all_of(age_cols),
                        names_to  = "age",
                        values_to = "value") %>%
    mutate(age = as.integer(sub("age", "", age)),
           metric=glue("{metric} {age}")) %>%
    select(-age)
}
