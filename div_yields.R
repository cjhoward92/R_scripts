library(httr)
library(stringr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(purrr)

get_price_url <- function(ticker) {
  str_interp("https://query1.finance.yahoo.com/v7/finance/download/${ticker}?period1=1521417600&period2=1679184000&interval=1d&events=history&includeAdjustedClose=true")
}

get_dividend_url <- function(ticker) {
  str_interp("https://query1.finance.yahoo.com/v7/finance/download/${ticker}?period1=1521417600&period2=1679184000&interval=1d&events=div&includeAdjustedClose=true")
}

download <- function(ticker) {
  price_url <- get_price_url(ticker)
  div_url <- get_dividend_url(ticker)

  price_name <- str_interp("${ticker}.csv")
  div_name <- str_interp("${ticker}_div.csv")


  if (!file_exists(price_name)) {
    download.file(price_url, price_name)
  }
  if (!file_exists(div_name)) {
    download.file(div_url, div_name)
  }
}

file_exists <- function(name) {
  full_path <- paste(getwd(), name, sep = "/")
  file.exists(full_path)
}

get_all_spx <- function() {
  spx <- read.csv("spx_constituents.csv")
  for (entry in 1:nrow(spx)) {
    ticker <- spx[entry, "Symbol"]
    print(str_interp("Downloading data for ${ticker}"))
    tryCatch(download(ticker), error = function(e) { print(str_interp("Error downloading ${ticker}")) })
    print(str_interp("Done downloading data for ${ticker}"))
    Sys.sleep(1)
  }
}

has.dividends <- function(ticker) {
  div_name <- str_interp("${ticker}_div.csv")

  if (!file_exists(div_name)) {
    return(FALSE)
  }

  divs <- suppressWarnings(read.csv(div_name))
  nrow(divs) > 0
}

# Vectorize to use in mutate
row.has.dividends <- Vectorize(has.dividends)

create.div.stock.csv <- function() {
  spx <- read.csv("spx_constituents.csv")
  has_divs <- spx %>%
    mutate(dividends = row.has.dividends(Symbol)) %>%
    filter(dividends) %>%
    transmute(ticker = Symbol)
  has_divs
}

# Algo:
# 1. Start with 100 shares
# 2. Load price data
# 3. When reach a payment date, calculate how many shares can be bought, keep leftover
# 4. Loop over each date
# 5. At end, calculate total $ in shares + remaining dividends

calculate.allocations <- function(.data, row) {
  if (row == 1) {
    return(.data)
  }

  .data$shares[row] <- .data$shares[row - 1]
  .data$remaining[row] <- (.data$shares[row] * .data$Dividends[row]) + (.data$remaining[row - 1])

  # Buy some shares...
  if (.data$remaining[row] > .data$Close[row]) {
    num_shares <- floor(.data$remaining[row] / .data$Close[row])
    total_price <- num_shares * .data$Close[row]

    if (num_shares == 0) {
      return(.data)
    }

    .data$remaining[row] <- .data$remaining[row] - total_price
    .data$shares[row] <- .data$shares[row] + num_shares
  }

  return(.data)
}

calculate.growth <- function(ticker, initial.shares = 100) {
  price_name <- str_interp("${ticker}.csv")
  div_name <- str_interp("${ticker}_div.csv")

  divs <- read.csv(div_name)
  print(summarize(divs, total = sum(Dividends)))
  prices <- read.csv(price_name) %>%
    mutate(shares = initial.shares,
           remaining = 0) %>%
    left_join(divs, by = "Date") %>%
    mutate(Dividends = coalesce(Dividends, 0))

  for (i in 1:nrow(prices)) {
    prices <- calculate.allocations(prices, i)
  }

  prices %>%
    mutate(present_value = (shares * Close) + remaining,
           total_dividencs = cumsum(Dividends * shares))
}
