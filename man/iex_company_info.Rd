% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iex.R
\name{iex_company_info}
\alias{iex_company_info}
\title{Returns information about the company.  This returns a list.
NOTE: We cannot simplify this list into a data.frame because some of the
fields (like 'tags' contain sub-lists that get replicated when we do this)}
\usage{
iex_company_info(symbol, simplify = TRUE)
}
\description{
Returns information about the company.  This returns a list.
NOTE: We cannot simplify this list into a data.frame because some of the
fields (like 'tags' contain sub-lists that get replicated when we do this)
}
