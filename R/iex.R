
# Base API Methods ------------------------------------------------------------

# TODO: these will need to be customizable when IEX introduces its IEX Cloud API.

iex_user_agent <- function() {
  httr::user_agent("Rpackage")
}

iex_base <- function(version) {
  "https://api.iextrading.com/"
}

iex_endpoint <- function(path, version = "1.0") {
  httr::modify_url(iex_base(), path = paste0(version, "/", path))
}

#' Base method that sends an HTTP GET request to the REST endpoint.  This returns
#' an S3 object of class `iex_api` which has three accessible fields:
#' `path`, `response` and `content` containing the API path, the unparsed API
#' response and the parsed content from the API's response (the latter usually
#' being a list). Note that this package causes R to pause 0.2 seconds after
#' executing an API call to avoid the user being throttled by the API (which
#' enforces a 5 request per second limit)
#'
#' @export
iex_api <- function(path, query = NULL, version = "1.0") {
  # End points throttled: 5 requests per second. Enforce this
  on.exit(Sys.sleep(0.2))
  resp <- httr::GET(iex_endpoint(path, version), query = query, iex_user_agent())
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
    simplifyVector = FALSE)
  if (httr::http_error(resp)) {
    stop(sprintf("IEX API request failed [%s]\n%s",
      httr::status_code(resp), parsed$error), call. = FALSE)
  }
  structure(list(content = parsed, path = path, response = resp),
    class = 'iex_api')
}

#' @export
print.iex_api <- function(x, ...) {
  cat("<IEX ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}

#' converts a list of atomic values to a 1-row data.frame
row_to_df <- function(content) {

  replace_null <- function(x) {
    ifelse(is.null(x), NA, x)
  }

  convert_row <- function(row) {
    lapply(row, replace_null)
  }

  content <- convert_row(content)
  as.data.frame(content, stringsAsFactors=F)
}

#' Converts a list of lists to a data.frame. This helps parse the typical
#' response$content list returned by most IEX functions.
#'
list_to_df <- function(content, ensure_columns=NULL) {

  replace_null <- function(x) {
    ifelse(is.null(x), NA, x)
  }

  convert_row <- function(row) {
    lapply(row, replace_null)
  }

  # we have to replace NULLs with NAs or else get the error:
  # 'arguments imply differing number of rows: 1, 0'
  content <- lapply(content, convert_row)

  # ensure that each row at least has these columns
  if (!is.null(ensure_columns)) {

    convert_row <- function(row) {
      missing_cols <- setdiff(ensure_columns, names(row))
      for (col in missing_cols) {
        row[[col]] <- NA
      }
      row
    }

    content <- lapply(content, convert_row)
  }

  bind <- function(...) {
    rbind.data.frame(..., stringsAsFactors=F)
  }

  do.call(bind, content)
}

# Reference Data --------------------------------------------------------------

#' Returns information about companies listed on the IEX exchange.
#' NOTE: Only 4 companies are returned so far (and 2 of these appear to be
#' tests).
#'
#' @export
iex_listed_symbols <- function(simplify=TRUE) {
  response <- iex_api("ref-data/daily-list/symbol-directory")
  if (!simplify) {return(response)}

  list_to_df(response$content)
}

#' Returns a list of all stock symbols traded on the IEX exchange.
#'
#' @param simplify Set to FALSE to get the raw API response.  When TRUE
#'        (the default) we simplify the result to a data.frame
#' @return A data frame containing the following columns:
#'          symbol - Stock symbol
#'          name - Stock name
#'          date - Most recent date (?)
#'          isEnabled
#'          type - "cs" = "common stock"
#'          iexId - Unique ID for the stock on the IEX exchange
#' @export
iex_traded_symbols <- function(simplify=TRUE) {
  response <- iex_api("ref-data/symbols")
  if (!simplify) {return(response)}

  list_to_df(response$content)
}

# Stocks ----------------------------------------------------------------------

#' Returns information about the company.  This returns a list.
#' NOTE: We cannot simplify this list into a data.frame because some of the
#' fields (like 'tags' contain sub-lists that get replicated when we do this)
#'
#' @export
iex_company_info <- function(symbol, simplify=TRUE) {
  response <- iex_api(paste0("stock/", symbol, "/company"))
  if (!simplify) {response}

  response$content
}

#' Returns all dividends declared by a company over a given timeframe. The
#' default time frame is 5 years.
#'
#' @export
iex_dividends <- function(symbol, timeframe="5y", simplify=TRUE) {
  response <- iex_api(paste0("stock/", symbol, "/dividends/", timeframe))
  if (!simplify) {return(response)}

  list_to_df(response$content)
}

#' Returns earnings data from the four most recent reported quarters.
#'
#' @export
iex_earnings <- function(symbol, simplify=TRUE) {
  response <- iex_api(paste0("stock/", symbol, "/earnings"))
  if (!simplify) {return(response)}

  list_to_df(response$content$earnings)
}

#' Pulls income statement, balance sheet, and cash flow data from the four most
#' recent reported quarters or years
#'
#' @export
iex_financials <- function(symbol, period=c("quarter", "annual"), simplify=TRUE) {
  query = list(
    period = match.arg(period)
  )
  response <- iex_api(sprintf("stock/%s/financials", symbol), query)
  if (!simplify) {return(response)}

  list_to_df(response$content$financials)
}

#' Returns all splits declared by a company over a given timeframe. The
#' default time frame is 5 years.
#'
#' @export
iex_splits <- function(symbol, timeframe="5y", simplify=TRUE) {
  response <- iex_api(paste0("stock/", symbol, "/splits/", timeframe))
  if (!simplify) {return(response)}

  list_to_df(response$content)
}

#' Returns historical chart data for the stock
#'
#' @export
iex_chart <- function(symbol, timeframe="5y", query=NULL, simplify=TRUE) {
  response <- iex_api(paste0("stock/", symbol, "/chart/", timeframe), query)
  if (!simplify) {return(response)}

  list_to_df(response$content,
             c("date", "open", "high", "low", "close", "volume", "unadjustedVolume",
               "change", "changePercent", "vwap", "label", "changeOverTime"))
}

#' Returns key statistics for the given stock
#'
#' @export
iex_stock_stats <- function(symbol, simplify=TRUE) {
  response <- iex_api(paste0("stock/", symbol, "/stats"))
  if (!simplify) {return(response)}

  response$content  # not much more we can do besides this
}

#' An array of peer tickers as defined by IEX. This is not intended to
#' represent a definitive or accurate list of peers, and is subject to
#' change at any time.
#'
#' @export
iex_stock_peers <- function(symbol, simplify=TRUE) {
  response <- iex_api(sprintf("stock/%s/peers", symbol))
  if (!simplify) {return(response)}

  as.character(response$content)
}

#' This returns previous day adjusted price data for a single stock.
#'
#' @export
iex_stock_previous <- function(symbol, simplify=TRUE) {
  response <- iex_api(sprintf("stock/%s/previous", symbol))
  if (!simplify) {return(response)}

  row_to_df(response$content)
}

#' This returns previous day adjusted price data for all stocks traded on IEX.
#'
#' @export
iex_market_previous <- function(simplify=TRUE) {
  response <- iex_api("stock/market/previous")
  if (!simplify) {return(response)}

  #list_to_df(response$content)  # doesn't work
  response$content
}

#' Similar to the peers endpoint, except this will return most active market
#' symbols when peers are not available.
#'
#' @export
iex_stock_relevant <- function(symbol, simplify=TRUE) {
  response <- iex_api(sprintf("stock/%s/relevant", symbol))
  if (!simplify) {return(response)}

  as.character(response$content$symbols)
}

#' This returns an array of each sector and performance for the current trading
#' day. Performance is based on each sector ETF.
#'
#' @export
iex_sector_performance <- function(simplify=TRUE) {
  response <- iex_api("stock/market/sector-performance")
  if (!simplify) {return(response)}

  list_to_df(response$content)
}

# This returns 15 minute delayed and 30 day average consolidated volume
# percentage of a stock, by market. This call will always return 13 values,
# and will be sorted in ascending order by current day trading volume
# percentage.
#
#' @export
iex_volume_by_venue <- function(symbol, simplify=TRUE) {
  response <- iex_api(paste0("stock/", symbol, "/volume-by-venue"))
  if (!simplify) {return(response)}

  list_to_df(response$content)  # not sure why this isn't working...
}


# TOPS --------------------------------------------------------------------

#' Top of book data (TOPS)
#'
#' Provides IEX's aggregated bid and offer position in near real time for
#' all securities on IEX's displayed limit order book.
#'
#' @param symbols a vector of tickers (case insensitive). Special characters
#'   will be escaped. A list of eligible symbols is
#'   [published daily](https://iextrading.com/trading/eligible-symbols/) by the
#'   IEX. When set to `NULL` (default) returns values for all symbols.
#' @param fields a vector of fields names to return (case sensitive). When
#'   set to `NULL` (default) returns values for all fields.
#' @param version the API version number (default: `"1.0"`) which is used to
#'   define the API URL.
#' @return an S3 object of class `iex_api` which has three accessible fields:
#'   `path`, `response` and `content` containing the API path, the unparsed API
#'   response and the parsed content from the API's response (the latter usually
#'   being a list). Note that this package causes R to pause 0.2 seconds after
#'   executing an API call to avoid the user being throttled by the API (which
#'   enforces a 5 request per second limit)
#' @examples
#' \dontrun{
#' tops(
#'   symbols = c("AAPL", "FB"),
#'   fields  = c("symbol", "bidSize", "bidPrice", "askSize", "askPrice")
#' )
#' }
#' @references [IEX API TOPS documentation](https://iextrading.com/developer/#tops-tops)
#' @export
tops <- function(symbols = NULL, fields = NULL, version = "1.0") {
  query <- list(
    symbols = paste0(symbols, collapse = ","),
    filter  = if (!is.null(fields)) paste0(fields, collapse = ",")
  )
  iex_api("tops", query, version)
}

#' Last trade data
#'
#' Provides IEX near real time last sale price, size and time. Last is ideal for
#' developers that need a lightweight stock quote.
#'
#' @inherit tops return params references
#' @examples
#' \dontrun{
#' last(
#'   symbols = c("AAPL", "FB"),
#'   fields  = c("symbol", "price", "size")
#' )
#' }
#' @export
last <- function(symbols = NULL, fields = NULL, version = "1.0") {
  query <- list(
    symbols = paste0(symbols, collapse = ","),
    filter  = if (!is.null(fields)) paste0(fields, collapse = ",")
  )
  iex_api("tops/last", query, version)
}


# Market ------------------------------------------------------------------

#' Market volume data
#'
#' Provides exchange trade volume data in near real time.
#'
#' @inherit tops params return
#' @examples
#' \dontrun{
#' market()
#' }
#' @references [IEX market API documentation](https://iextrading.com/developer/#market-market)
#' @export
market <- function(fields = NULL, version = "1.0") {
  query <- list(
    filter = if (!is.null(fields)) paste0(fields, collapse = ",")
  )
  iex_api("market", query, version)
}


# Stats -------------------------------------------------------------------

#' Trading stats
#'
#' A set of functions that return trading statistics.
#'
#' @inherit tops params return
#' @param date should be a string of the format `"YYYYMM"` or `"YYYYMMDD"` which
#'   is a valid option for `daily_stats()` or `NULL` (default) which returns the
#'   prior trading date's data for `daily_stats()`  and the prior month's
#'   trading data for `monthly_stats()`
#' @param last can be used in place of `date` to retrieve the last `n` number of
#'   trading days' data. If this is supplied, any value supplied to `date` is
#'   ignored.
#' @references [IEX stats API documentation](https://iextrading.com/developer/#stats)
#' @name stats
NULL

stats <- function(type, date = NULL, last = NULL, fields = NULL, version = "1.0") {
  path <- paste("stats", type, sep = "/")
  query <- list(
    date = date,
    last = last,
    filter = if (!is.null(fields)) paste0(fields, collapse = ",")
  )
  iex_api(path, query, version)
}

#' @rdname stats
#' @export
intraday_stats <- function(fields = NULL, version = "1.0") {
  stats("intraday", fields = fields, version = version)
}

#' @rdname stats
#' @export
recent_stats <- function(fields = NULL, version = "1.0") {
  stats("recent", fields = fields, version = version)
}

#' @rdname stats
#' @export
records_stats <- function(fields = NULL, version = "1.0") {
  stats("records", fields = fields, version = version)
}

#' @rdname stats
#' @export
monthly_stats <- function(date = NULL, fields = NULL, version = "1.0") {
  stats("historical", date = date, fields = fields, version = version)
}

#' @rdname stats
#' @export
daily_stats <- function(date = NULL, last = NULL, fields = NULL, version = "1.0") {
  if (!is.null(last)) date <- NULL
  stats("historical/daily", date, last, fields, version)
}
