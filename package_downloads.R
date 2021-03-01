# API DEVELOPMENT WORKFLOW ----
# Analysis of most popular R package downloads
#
# 1. Libraries ----
# install.packages("cranlogs")
# install.packages("cran_top_downloads")

library(tidyverse)
library(timetk)
library(cranlogs)
library(forcats)

?cranlogs

# 2. Get Downloads ----

r_downloads <- cran_downloads("R", from = "2011-01-01") %>%
  as_tibble()

r_downloads %>% glimpse()

r_downloads %>%
  mutate(version = as_factor(version)) %>%
  group_by(version) %>%
  summarise_by_time(date, .by = "month", count = sum(count)) %>%
  plot_time_series(.date_var = date, .value = count,
                   .color_var = version, .smooth = FALSE,
    .title = "Monthly Downloads of R by Version"
  )


package_downloads <- cran_downloads(c("timetk", "tidymodels",
                                      "modeltime", "caret", "h2o", "forecast",
                                      "parsnip", "prophet", "fpp3"),
                                    from = "2011-01-01") %>%
  as_tibble()

# 3. Visualisations ----

package_downloads %>%
  group_by(package) %>%
  summarise_by_time(date, .by = "week", count = sum(count)) %>%
  plot_time_series(.date_var = date, .value = count,
                   # .facet_collapse = TRUE,
                   .facet_vars = package, .smooth_period = 365,
                   .facet_ncol = 2,
                   # .color_var = package,
                   .title = "Weekly Downloads of timetk package"
  )
