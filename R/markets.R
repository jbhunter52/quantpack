
#' Get NYSE turn of month trading days indicator
#'
#' @param from Start date
#' @param to End Date
#' @param ndays Count down/up days
#' @return data.frame
#' @export
get_turn_month_days <- function(from, to, ndays)
{
  all_dates <- seq.Date(from=from %m-% months(2), to=to %m+% months(2), by='days')
  holidays <- RQuantLib::getHolidayList('UnitedStates/NYSE', from=min(all_dates), to=max(all_dates))
  businessday <- RQuantLib::getBusinessDayList('UnitedStates/NYSE', from=min(all_dates), to=max(all_dates))
  base_days <- data.frame(
    date = all_dates
  ) |>
    mutate(day = lubridate::mday(date),
           holiday = date %in% holidays,
           bd = date %in% businessday,
           trading_day = bd & !holiday)

  trading_days <- base_days |>
    mutate(year = lubridate::year(date),
           month = lubridate::month(date)) |>
    filter(trading_day) |>
    group_by(year, month) %>%
    arrange(date) %>%
    mutate(rn = row_number(),
           nr = n(),
           nr_n = nr-ndays,
           month_day = case_when(
             rn <= nr_n ~ rn,
             rn > nr_n ~ rn-nr - 1L,
             TRUE ~ as.integer(NA))) |>
    ungroup() |>
    select(date, month_day) |>
    filter(between(date,from,to))
  return (trading_days)

}
