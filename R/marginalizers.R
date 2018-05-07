#' Aggregate conditional pdfs into a marginal pdf
#' @param pdfs list of pdfs
#' @param probs numeric vector (or list) of probabilities the same length as \code{pdf_list}
#' @return marginal pdf (returned as a function)
#' @export
aggregate_pdfs <- function(pdfs, probs) {

  prob_sum <- sum(probs)

  if(!(near(prob_sum, 1, tol = 1e-3)))
    warning(sprintf('total_prob = %.3f', prob_sum))

  function(x) {
    n <- length(x)
    vapply(pdfs, function(f) f(x), numeric(n)) %*%
      probs %>% as.vector()}}

#' Integrate (marginal) pdf to get cdf (as a function)
#' @param pdf marginal pdf to be integrated
#' @export
integrate_pdf <- function(pdf) {
  function(x) {
    vapply(x,
      function(x)
        integrate(pdf, lower = 0, upper = x,
          subdivisions = 999L,
          stop.on.error = FALSE)$value,
      numeric(1))}}

# Integrate (marginal) pdf to get cdf (as a function)
#
# @param pdf pdf to be integrated
integrate_pdf2 <- function(pdf) {
  function(x) {
    map_dbl(x,
      ~integrate(pdf, lower = 0, upper = .,
        subdivisions = 999L,
        stop.on.error = FALSE)$value)}}

#' Return reorder-point function by inverting cdf
#' @param cdf Marginal lead-time demand cdf, as a _function_
#' @return A function that returns a two-column tibble with
#'     vars: \code{svc_lvl} and \code{rop}
#' @export
rop_fun <- function(cdf) {
  function(svc_lvl) {
    tibble(
      svc_lvl,
      rop = map_dbl(svc_lvl,
        ~uniroot(function(x) {cdf(x) - .},
          c(0, 111), extendInt = 'upX')$root))}}
