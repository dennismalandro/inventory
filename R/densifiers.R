
kde <- function(x, bw = 'bcv', ...)
  density(x, bw = bw, ...) %>%
  broom::tidy() %>%
  tbl_df()

ash <- function(x, m = 15, nbin = 180, beta = 0.2)
  tibble(x = list(x)) %>%
  mutate(
    range = map(x, ash:::nicerange, beta),
    bin1 = map2(x, range, ash::bin1, nbin),
    ash1 = map(bin1, ash::ash1, m),
    # range = map(x, ash:::nicerange, args$beta),
    # bin1 = map2(x, range, ash::bin1, args$nbin),
    # ash1 = map(bin1, ash::ash1, args$m),
    # h = map2_dbl(bin1, ash1,
    #  ~diff(.x$ab) / length(.x$nc) * .y$m),
    x = map(ash1, 'x'),
    y = map(ash1, 'y')) %>%
  select(x, y) %>%
  unnest()

# Reference Wand, Jones for end effects picture

#' Calculate the nonparametric density (safely) of a numeric vector.
#'
#' @param x numeric vector
#' @param type the type of density estimator (kde or ash)
#' @param trans transfrom to be applied to \code{x} before estimation (sqrt, log, none, reflect)
#' @return a density as a tibble of \code{x}, \code{y} pairs
safely_densify <- safely(
  function(x,
    type = c('ash', 'kde'),
    trans = c('sqrt', 'log', 'none', 'reflect'),
    ...) {

    type <- match.arg(type)
    trans <- match.arg(trans)

    if(trans == 'reflect') x <- c(x, -x)
    if(trans == 'log') x <- log(x)
    if(trans == 'sqrt') x <- sqrt(x)

    dots <- list(...)
    dots$x <- x
    if(trans == 'reflect') dots$n <- 2 * 512

    dns <- switch(type,
     'ash' = do.call('ash', dots),
     'kde' = do.call('kde', dots))


    if(trans == 'reflect')
      dns <- dns %>%
        filter(x >= 0) %>%
        mutate(y = 2 * y)

    if(trans == 'log')
      dns <- dns %>%
        mutate(
          x = exp(x),
          y = y / x)

    if(trans == 'sqrt')
      dns <- dns %>%
         mutate(
           x = x^2,
           y = y / (2 * sqrt(x)))

    dns})

#' Calculate the nonparametric density of a numeric vector.
#'
#' @param x numeric vector
#' @param type the type of density estimator (kde or ash)
#' @param trans transfrom to be applied to \code{x} before estimation (sqrt, log, none, reflect)
#' @return a density as a tibble of \code{x}, \code{y} pairs
#' @export
densify <- function(x,
    type = c('ash', 'kde'),
    trans = c('sqrt', 'log', 'none', 'reflect'),
    ...) {

    type <- match.arg(type)
    trans <- match.arg(trans)

    if(trans == 'reflect') x <- c(x, -x)
    if(trans == 'log') x <- log(x)
    if(trans == 'sqrt') x <- sqrt(x)

    dots <- list(...)
    dots$x <- x
    if(trans == 'reflect') dots$n <- 2 * 512

    dns <- switch(type,
     'ash' = do.call('ash', dots),
     'kde' = do.call('kde', dots))


    if(trans == 'reflect')
      dns <- dns %>%
        filter(x >= 0) %>%
        mutate(y = 2 * y)

    if(trans == 'log')
      dns <- dns %>%
        mutate(
          x = exp(x),
          y = y / x)

    if(trans == 'sqrt')
      dns <- dns %>%
         mutate(
           x = x^2,
           y = y / (2 * sqrt(x)))

    dns}



# Not sure what I was thinking here
# density_funs <- tibble(
#   fun = list(kde = kde, ash = ash),
#   type = names(fun),
#   param = list(
#     list(bw = 'bcv'),
#     list(m = 15, nbin = 180, beta = 0.2)))


# Make a pmf from of the data (from a histogram)
data_to_hist_pmf <- function(x) {
  hist <- hist(x = x, plot = FALSE)
  breaks <- hist$breaks
  y <- hist$density

  tibble(
    x = hist$mids,
    prob = diff(breaks) * y,
    cum_prob = cumsum(prob)) %>%
    filter(prob > 0)}


#' Make a pmf from a density
#' @param tbl a density object as a tibble of \code{x}, \code{y} pairs
#' @return a pmf tibble of \code{x}, \code{prob}, and \code{cumprob}
#' @export
density_to_pmf <- function(tbl) {

  # from spatial::surf.gls
  if (is.data.frame(tbl)) {
    if (any(is.na(match(c("x", "y"), names(tbl)))))
      stop("'tbl' does not have columns 'x', 'y'")}

  x <- tbl[['x']]
  y <- tbl[['y']]
  tibble(
    dx = lead(x) - x,
    x_mid = (lead(x) + x) / 2,
    y_mid = (lead(y) + y) / 2,
    prob = dx * y_mid,
    cum_prob = cumsum(prob)) %>%
    drop_na() %>%
    select(x = x_mid, prob, cum_prob) %>%
    filter(prob > 0)}


#' Rescale pdf, f_X to f_Y where y = rx,
#' @param rate the factor by which to rescale the pdf
#' @param pdf the pdf to be rescaled, as a _function_
#' @return a rescaled pdf as a _function_
#' @export
rescale_pdf <- function(rate, pdf)
  function(y) pdf(y / rate) / rate


