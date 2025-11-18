# Energy consumption in the UK in 2000

A dataset containing approximations to some of the energy flows in the
UK in the year 2000. These data first appeared as the example in Figures
3, 7, and 11 of M.K. Heun, A. Owen, and P.E. Brockway. 2018. A physical
supply-use table framework for energy analysis on the energy conversion
chain. Applied Energy, Vol. 226, pp. 1134-1162.

## Usage

``` r
UKEnergy2000
```

## Format

A data frame with 36 rows and 7 variables:

- Country:

  country, GB (Great Britain, only one country)

- Year:

  year, 2000 (only one year)

- Ledger.side:

  Supply or Consumption

- Flow.aggregation.point:

  tells where each row should be aggregated

- Flow:

  the Industry or Sector involved in this flow

- Product:

  the energy product involved in this flow

- E.ktoe:

  magnitude of the energy flow in ktoe

## Source

[doi:10.1016/j.apenergy.2018.05.109](https://doi.org/10.1016/j.apenergy.2018.05.109)
