#' Compute daily demand distribution
#'
#' @param .data A tibble containing (at least) the following dates and quantities
#' @param date Quoted column name within `.data` referring to the Date on which demand is to be met (and hence, inventory consumed)
#' @param qty Quoted column name within `.data` referring to the quantity orded (_i.e._, _demanded_)
#' @return A frequency tibble of daily demand
#' @export
tableize_demand <- function(.data, date, qty) {
  date <- enexpr(date)
  qty <- enexpr(qty)
  qty_name <- expr_name(qty)

  # Total number of days spanned in the data
  date_range <- .data %>%
    pull(!!date) %>%
    range() %>%
    diff() %>%
    as.numeric() + 1

  # Number of dates on which orders were placed
  n_dates <- .data %>%
    pull(!!date) %>%
    n_distinct()

  nzeros <- date_range - n_dates

  .data %>%
    group_by(!!date) %>%
    summarize(!!qty_name := sum(!!qty)) %>%
    arrange(!!date) %>%
    count(!!qty) %>%
    add_row(!!qty_name := 0, n = nzeros, .before = 1)}


#' Generate demand-during-lead-time distribution
#'
#' @param lead_times A numeric vector of lead times in days
#' @param demand_freq A frequency tibble of daily demands (normally gotten as output from tableize_demand()`)
#' @param cycles Number of 'saw tooth' inventory replenishment cycles to simulate
#' @return A numeric vector of demands during lead time, of length `cycles`
#' @export
generate_ddlt <- function(
  lead_times, demand_freq, cycles = 999) {
  # lt_vector - vector of lead times
  # dmd_tbl - table of order quantities and weights
  # qty <- enexpr(qty)

  rerun(cycles, {
    lt <- sample(lead_times, size = 1)
    demand_freq %>%
      sample_n(weight=n, size = round(lt), replace = T) %>%
      pull(-n) %>%
      sum()}) %>%
    unlist()}


#' Compute demand rates from demand epochs and quantities
#'
#' @param .data A tibble containing (at least) the following dates and quantities
#' @param date Epoch at which an order is placed
#' @param qty Quantity orded
#' @return A tibble of demand rates by item
#' @export
demand_rates <- function(.data, date, qty) {
  date <- enexpr(date)
  qty <- enexpr(qty)
  qty_name <- expr_name(qty)

 .data %>%
    group_by(!!date) %>%
    summarize(!!qty_name := sum(!!qty)) %>%
    arrange(!!date) %>%
    mutate(
     'Interorder Duration' =
        interval(lag(!!date), !!date) %>%
          as.numeric('days'),
     'Demand Rate' = !!qty / `Interorder Duration`) %>%
   drop_na()}


#' Compute lead times from work-order dates and completed dates
#'
#' @param .data A tibble containing (at least) dates for work order placement and completion
#' @param start Date, start of lead time
#' @param end Date, end of lead time
#' @param name String, name of returned lead-time column
#' @return .data with lead-time column appended
#' @export
lead_times <- function(.data, start, end, name = 'Lead Time') {
  start <- enexpr(start)
  end <- enexpr(end)

  .data %>%
    mutate(
      !!name := interval(!!start, !!end) %>%
        as.numeric('days'))}
