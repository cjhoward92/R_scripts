library(tidyverse)

download.yields <- function(from, to) {
  print(str_interp("Downloading T-Bill par yields from ${from} to ${to}"))
  for (i in from:to) {
    print(str_interp("Starting download for ${i}"))
    url <- paste("https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/", i, "/all?type=daily_treasury_yield_curve&field_tdr_date_value=", i, "&page&_format=csv", sep = "")
    out_file <- paste("yields_", i, ".csv", sep = "")
    download.file(url, out_file)
    print(str_interp("Finished download for ${i}"))
  }
  print("All done!")
}

read.yields <- function(year) {
  file_name <- str_interp("yields_${year}.csv")
  return(read.csv(file_name))
}

clean.yields <- function(data) {
  clean <- data %>%
    mutate(clean_date = as.Date(Date, format = "%m/%d/%y"),
           year = year(clean_date),
           month = month(clean_date))
  return(clean)
}

graph.yields <- function(data) {
  ggplot(data, aes(x=clean_date, y=X3.Mo, group=month)) +
    geom_point() +
    stat_smooth()
}

bind.yields <- function(...) {
  all_data <- bind_rows(...)
  return(all_data)
}