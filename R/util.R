#' Calculate demand rates from demand epochs and quantities
#'
#' @param .data A tibble containing (at least) the following dates and quantities
#' @param item item id
#' @param date Epoch at which an order is placed
#' @param qty Quantity orded
#' @param dr_name Returned demand-rate variable name
#' @return A tibble of demand rates by item
#' @export
demand_rates <- function(.data, item, date, qty) {
  item <- enexpr(item)
  date <- enexpr(date)
  qty <- enexpr(qty)
  item_name <- quo_name(item)
  qty_name <- quo_name(qty)
  # grp_vars <- exprs(..., .named = TRUE)

  .data %>%
    group_by(!!item, !!date) %>%
    summarize(!!qty_name := sum(!!qty)) %>%
    arrange(!!item, !!date) %>%
    mutate(
      'Interorder Duration' = interval(lag(!!date), !!date) %>%
        as.numeric('days'),
      'Demand Rate' = !!qty / `Interorder Duration`) %>%
    drop_na()}


#' Calculate lead times from work-order dates and completed dates
#'
#' @param .data A tibble containing (at least) dates for work order placement and completion
#' @param item Item id
#' @param lt_name Returned lead-time variable name
#' @return .data with lead-time column appended
#' @export
lead_times <- function(.data, start, end) {
  start <- enexpr(start)
  end <- enexpr(end)

  .data %>%
    mutate(
      'Lead Time' = interval(!!start, !!end) %>%
        as.numeric('days'))}
