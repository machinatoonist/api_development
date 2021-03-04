# Load the httr package
library(httr)
library(jsonlite)
library(tidyverse)
library(timetk)
library(lubridate)
library(cranlogs)
library(plumber)

# HTTP request and response testing service example ----

# Make a GET request to http://httpbin.org/get
get_result <- GET("http://httpbin.org/get")

# Print it to inspect it
get_result
content(get_result)

# Make a POST request to http://httpbin.org/post with the body "this is a test"
post_result <- POST("http://httpbin.org/post", body = "this is a test")

# Print it to inspect it
post_result
content(post_result)

# Wikimedia example ----

# Directory-based URLs
url <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia.org/all-access/all-agents/Hadley_Wickham/weekly/20170101/20200102"
url2 <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/top/en.wikipedia.org/all-access/2021/03/01"

# Parameter-based URLs

# Make a GET request to url and save the results
pageview_response <- GET(url)

# Call content() to retrieve the data the server sent back
pageview_data <- content(pageview_response)

# Examine the results with str()
str(pageview_data)


# Handling errors ----
fake_url <- "http://google.com/fakepagethatdoesnotexist"

# Make the GET request
request_result <- GET(fake_url)

# Check request_result
if(http_error(request_result)){
  warning("The request failed")
} else {
  content(request_result)
}


# Constructing queries ----
# Construct a directory-based API URL to `http://swapi.co/api`,
# looking for person `1` in `people`
directory_url <- paste("http://swapi.co/api", "people", 1, sep = "/")

# Make a GET call with it
result <- GET(directory_url)

# Construct a parameter based URL
# Create list with nationality and country elements
query_params <- list(nationality = "americans",
                     country = "antigua")

# Make parameter-based call to httpbin, with query_params
parameter_response <- GET("https://httpbin.org/get", query = query_params)

# Print parameter_response
parameter_response

# Respectful API usage: user agents ----
# Do not change the url
url3 <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/Aaron_Halfaker/daily/2015100100/2015103100"

# Add the email address and the test sentence inside user_agent()
server_response <- GET(url3, user_agent("my@email.address this is a test"))


# Construct a vector of 2 URLs
urls <- c("http://httpbin.org/status/404", "http://httpbin.org/status/301")

for(url in urls){
  # Send a GET request to url
  result <- GET(url)
  # Delay for 5 seconds between requests
  Sys.sleep(5)
}

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



