# R API for IEX

Library in R to access data [IEX's API](https://iextrading.com/developer/docs/).  
This can retrieve company information, historical prices, dividends, splits,
key statistics, and several other data sets from IEX.

Here is a sample of some functions in this library:

```
listed_symbols <- iex_listed_symbols()
traded_symbols <- iex_traded_symbols()

symbol <- "KR"

info <- iex_company_info(symbol)
key_stats <- iex_stock_stats(symbol)

df.dividends <- iex_dividends(symbol)
df.splits <- iex_splits(symbol)
df.chart <- iex_chart(symbol)

plot(df.chart$close, type="l")

volume <- iex_volume_by_venue(symbol)
```

This is a fork of the library by [imanuelcostigan](https://github.com/imanuelcostigan/iex).
It has been modified in the following ways:

* Adding support for several new endpoints
* Adding a 'simplify' parameter to convert response data to data.frames automatically
* Exporting the ```iex_api``` function to make it easier to call endpoints not covered by this library

This is a work in progress.  I plan to expand this in the next few weeks with support
for additional endpoints.  I also plan to support the new IEX Cloud API once it is
released.
