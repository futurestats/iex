% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iex.R
\name{tops}
\alias{tops}
\title{Top of book data (TOPS)}
\usage{
tops(symbols = NULL, fields = NULL, version = "1.0")
}
\arguments{
\item{symbols}{a vector of tickers (case insensitive). Special characters
will be escaped. A list of eligible symbols is
\href{https://iextrading.com/trading/eligible-symbols/}{published daily} by the
IEX. When set to \code{NULL} (default) returns values for all symbols.}

\item{fields}{a vector of fields names to return (case sensitive). When
set to \code{NULL} (default) returns values for all fields.}

\item{version}{the API version number (default: \code{"1.0"}) which is used to
define the API URL.}
}
\value{
an S3 object of class \code{iex_api} which has three accessible fields:
\code{path}, \code{response} and \code{content} containing the API path, the unparsed API
response and the parsed content from the API's response (the latter usually
being a list). Note that this package causes R to pause 0.2 seconds after
executing an API call to avoid the user being throttled by the API (which
enforces a 5 request per second limit)
}
\description{
Provides IEX's aggregated bid and offer position in near real time for
all securities on IEX's displayed limit order book.
}
\examples{
\dontrun{
tops(
  symbols = c("AAPL", "FB"),
  fields  = c("symbol", "bidSize", "bidPrice", "askSize", "askPrice")
)
}
}
\references{
\href{https://iextrading.com/developer/#tops-tops}{IEX API TOPS documentation}
}
