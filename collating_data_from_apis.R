# GETTING DATA FROM APIs FOR ANALYSIS
# Author: Matt Rosinski
# Email: mattrosinski@gmail.com
# Date: March 5, 2021
#
# Load libraries ----
library(httr)
library(jsonlite)
library(tidyverse)
library(timetk)
library(lubridate)
library(cranlogs)
library(plumber)

# Get Wikipedia Article Downloads Function ----

get_pageviews <- function(article_title, start_date_ymd, end_date_ymd){
  url <- paste(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents",
    article_title,
    "daily",
    start_date_ymd,
    end_date_ymd,
    sep = "/"
  )
  response <- GET(url, user_agent("matt.rosinski@interlate.com.au this is a test"))
  # Is there an HTTP error?
  if(http_error(response)){
    # Throw an R error
    stop("the request failed")
  }
  # Return the response's content
  content(response, as = "text", simplifyVector = FALSE)

}

# Get CRAN Downloads Function ----

get_cran_data <- function(package = "tidyverse", start = "2011-01-01",
                          end = lubridate::today(), by = "day") {
  if (!by %in% c("day", "week", "month", "quarter", "year")) {
    by <- "day"
  }
  # cran_downloads() issues the following request to the RStudio CRAN API
  # req <- GET(paste0(daily_url, interval, ppackages))
  # fromJSON(content(req, as = "text"), simplifyVector = FALSE)

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

# Get data from APIs and join ----


wikipedia_data_raw <- get_pageviews("Hadley_Wickham", "2015100100", "2021030100")


package <- c("tidyverse", "dplyr")
start <- "2015-01-01"
end <- lubridate::today(tzone = "UTC")
by <-  "day"

cran_data_raw <- get_cran_data(package, start, end, by)

cran_data_prep_tbl <- cran_data_raw %>%
  pivot_wider(values_from = count, names_from = package)

# Format and join data ----

wikipedia_data_prepared_tbl <- fromJSON(wikipedia_data_raw) %>% as_tibble()


wikipedia_data_prepared_tbl <- wikipedia_data_prepared_tbl$items %>%
  mutate(views = as.numeric(views),
         timestamp = ymd_h(timestamp)) %>%
  relocate(timestamp) %>%
  summarise_by_time(.date_var = timestamp, .by = "week", views = sum(views))

cran_data_prepared_tbl <- cran_data_prep_tbl %>%
  summarise_by_time(.date_var = date, .by = "week", dplyr_downloads = sum(dplyr), tidyverse_downloads = sum(tidyverse))

summary_tbl <- wikipedia_data_prepared_tbl %>%
  inner_join(cran_data_prepared_tbl, by = c("timestamp" = "date"))

summary_tbl %>%
  filter(tidyverse_downloads < 400000) %>%
  pivot_longer(views:tidyverse_downloads) %>%
  plot_time_series(timestamp, value, .facet_vars = name, .smooth = FALSE,
                   .title = "Comparison Of Weekly Wikipedia Article Views about Hadley Wickham and Downloads of Tidyverse Downloads"
                   )



