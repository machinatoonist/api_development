# PLUMBER API ----
#
# 1.0 LIBRARIES ----

library(plumber)
library(tidyverse)
library(timetk)
library(cranlogs)

# 2.0 MAIN FUNCTION ----

get_cran_data <- function(package = "dplyr", start = "2011-01-01",
                          end = lubridate::today(), by = "day") {
  if (!by %in% c("day", "week", "month", "quarter", "year")) {
    by <- "day"
  }

  data_raw_tbl <- cran_downloads(
    packages = package,
    from = as.character(start),
    to = as.character(end)
  ) %>% as_tibble()

  data_prepared_tbl <- data_raw_tbl %>%
    group_by(package) %>%
    summarize_by_time(.date_var = date,
                      .by = by, count = sum(count, na.rm = TRUE))

  return(data_prepared_tbl)
}


# 3.0 ENDPOINTS ----

#* @apiTitle Downloads API

#  * /cran ----
#
#* Get the raw data download
#*
#* @param package The package to get download history for
#* @param start The start date for downloads
#* @param end The end date for downloads
#* @param by The time aggregation to use
#*
#* @get /cran
function(package = "dplyr", start = "2011-01-01",
         end = lubridate::today(), by = "day") {
  list(
    data = get_cran_data(package, start, end, by)
    )
}

#  * /cran/timeplot ----

#* Return a plotly time series plot
#*
#* @param package The package to get download history for
#* @param start The start date for downloads
#* @param end The end date for downloads
#* @param by The time aggregation to use
#* @show_smooth TRUE/FALSE. Toggle the smoother on and off.  Default is on.
#*
#* @get /cran/timeplot
#* @serializer htmlwidget
function(package = "dplyr", start = "2017-01-01",
         end = lubridate::today(tzone = "UTC"), by = "day", show_smooth = TRUE,
         smooth_span = 0.75) {

  data_prepared_tbl <- get_cran_data(package, start, end, by)

  data_prepared_tbl %>%
    group_by(package) %>%
    summarise_by_time(date, .by = by, count = sum(count)) %>%
    plot_time_series(.date_var = date, .value = count,
                   .smooth = as.logical(show_smooth),
                   .smooth_span = as.numeric(smooth_span),
                   .title = str_glue("Downloads Time Series for {package} Package")
  )
}