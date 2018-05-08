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
#' @return .data with lead-time column appended
#' @export
lead_times <- function(.data, start, end) {
  start <- enexpr(start)
  end <- enexpr(end)

  .data %>%
    mutate(
      'Lead Time' = interval(!!start, !!end) %>%
        as.numeric('days'))}
