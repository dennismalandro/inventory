# Convert dmd epoch/size tbl to dmd-rate tbl
#' Calculate the nonparametric density of a numeric vector.
#'
#' @param .data tibble containing dates and quantities
#' @param date order-date column
#' @param qty order-quantity column
#' @param ... grouping variable(s) (eg item)
#' @return a demand-rate transformed tibble of (eg, demand) rates
#' @export
orders_to_rates <- function(.data, date, qty, ...) {
  date <- enexpr(date)
  qty <- enexpr(qty)
  qty_name <- quo_name(qty)
  grp_vars <- exprs(..., .named = TRUE)

 .data %>%
    mutate_if(is.POSIXt, as.Date) %>%
    group_by(!!!grp_vars, !!date) %>%
    summarize(!!qty_name := sum(!!qty)) %>%
    arrange(!!!grp_vars, !!date) %>%
    mutate(
     `Interorder Duration` = interval(lag(!!date), !!date) %>%
        as.numeric('days'),
     `Demand Rate` = !!qty / `Interorder Duration`) %>%
    drop_na()}
