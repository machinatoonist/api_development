# GETTING DATA FROM Hacker News API
# Author: Matt Rosinski
# Email: mattrosinski@gmail.com
# Date: March 13, 2021
#
# Load libraries ----
library(httr)
library(jsonlite)
library(tidyverse)
library(timetk)
library(lubridate)
library(cranlogs)
library(plumber)
library(glue)
library(stringr)

# Get Hacker News Information Function ----
#
url_base <- "https://hacker-news.firebaseio.com/v0/item/"

url <- "https://hacker-news.firebaseio.com/v0/item/8863.json?print=pretty"

url_job <- "https://hacker-news.firebaseio.com/v0/askstories.json?print=pretty"

response <- GET(url, user_agent("mattrosinski@gmail.com this is a test"))

job_response <- GET(url_job, user_agent("mattrosinski@gmail.com this is a test"))

article_info <- content(response, as = "text", simplifyVector = FALSE)

article_info_prepared_tbl <- fromJSON(article_info) %>% as_tibble()

job_stories <- content(job_response, as = "text", simplifyVector = FALSE)

job_stories_prepared_tbl <- fromJSON(job_stories) %>% as_tibble()

job_content_tbl <- job_stories_prepared_tbl %>%
  mutate(
    job_url = glue("{url_base}{value}.json")
    )

content = fromJSON(content(GET(job_url), as = "text", simplyVector = FALSE))

glue("{job_content_tbl$job_url[1]}")
job_content_tbl$job_url[1]
content = fromJSON(content(GET(job_content_tbl$job_url[1]), as = "text",
                           simplyVector = FALSE)) %>% as_tibble()

# incorporate the story ids with GET to collate text from job_stories
#
# The current largest item id is at /v0/maxitem. You can walk backward from
# here to discover all items.

max_url <-  "https://hacker-news.firebaseio.com/v0/maxitem.json"

max_response <- GET(max_url, user_agent("mattrosinski@gmail.com this is a test"))

max_response_value <- content(max_response, as = "text", simplifyVector = FALSE)

construct_url <- glue("{url_base}{max_response_value}.json")

max_response <- GET(construct_url, user_agent("mattrosinski@gmail.com this is a test"))

max_content <- content(max_response, as = "text", simplyVector = FALSE)

max_content_tbl <- fromJSON(max_content) %>% as_tibble()

