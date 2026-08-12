#' Sample a positive mean via log-normal method of moments
#'
#' Draws a single value from a log-normal distribution matching a target mean
#' and variance on the natural scale, subject to a lower-bound threshold.
#'
#' @details
#' Converts natural-scale parameters $(\mu, v)$ to log-scale parameters
#' $(\mu_{\log}, \sigma_{\log})$ using standard method of moments:
#' $$\sigma^2_{\log} = \ln\left(1 + \frac{v}{\mu^2}\right)$$
#' $$\mu_{\log} = \ln(\mu) - \frac{1}{2}\sigma^2_{\log}$$
#'
#' Edge cases are handled deterministically:
#' * If `mu` is non-finite or $\le 0$, the function returns `min_mu`.
#' * If `var_mu` is non-finite or $\le 0$, uncertainty is ignored and `mu` is returned.
#'
#' @param mu Numeric scalar. Arithmetic mean on the natural scale.
#' @param var_mu Numeric scalar. Variance on the natural scale.
#' @param min_mu Numeric scalar. Lower bound floor for the output. Default is `1e-8`.
#'
#' @return Numeric scalar representing a single positive random sample bounded below by `min_mu`.
#'
#' @importFrom stats rlnorm
#' @export
sample_positive_mean <- function(mu, var_mu, min_mu = 1e-8) {

  mu     <- as.numeric(mu)[1]
  var_mu <- as.numeric(var_mu)[1]

  if (!is.finite(mu) || mu <= 0) {
    return(min_mu)
  }

  if (!is.finite(var_mu) || var_mu <= 0) {
    return(mu)
  }

  sigma2_log <- log1p(var_mu / mu^2)
  meanlog     <- log(mu) - 0.5 * sigma2_log
  sdlog       <- sqrt(sigma2_log)

  sampled_mu <- stats::rlnorm(
    n = 1,
    meanlog = meanlog,
    sdlog = sdlog
  )

  max(sampled_mu, min_mu)
}

