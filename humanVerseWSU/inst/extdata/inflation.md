# "inflation" data set

<https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000> Downloaded in September 2020 using `RCurl` and parsed using `rvest` library.

The following data has `$1,000,000` established in 1920, and for each year (up to 2020) has an adjusted dollar amount.

For example, after the Great Depression, in 1933, it was only `$650,000`.

One can run `loadInflationData(); plot(inflation.df, type="b", pch="+");` to see the overall trend of the data.


