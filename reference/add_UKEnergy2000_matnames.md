# Add a column of matrix names to tidy data frame

Add a column of matrix names to tidy data frame

## Usage

``` r
add_UKEnergy2000_matnames(
  .DF,
  ledger_side_colname = "Ledger.side",
  energy_colname = "E.ktoe",
  supply_side = "Supply",
  consumption_side = "Consumption",
  matname_colname = "matname",
  U_name = "U",
  V_name = "V",
  Y_name = "Y"
)
```

## Arguments

- .DF:

  a data frame with `ledger_side_colname` and `energy_colname`.

- ledger_side_colname:

  the name of the column in `.DF` that contains ledger side (a string).
  Default is "`Ledger.side`".

- energy_colname:

  the name of the column in `.DF` that contains energy values (a
  string). Default is "`E.ktoe`".

- supply_side:

  the identifier for items on the supply side of the ledger (a string).
  Default is "`Supply`".

- consumption_side:

  the identifier for items on the consumption side of the ledger (a
  string). Default is "`Consumption`".

- matname_colname:

  the name of the output column containing the name of the matrix in
  which this row belongs (a string). Default is "`UVY`".

- U_name:

  the name for the use matrix (a string). Default is "`U`".

- V_name:

  the name for the make matrix (a string). Default is "`V`".

- Y_name:

  the name for the final demand matrix (a string). Default is "`Y`".

## Value

`.DF` with an added column, `UVY_colname`.

## Examples

``` r
matsindf:::add_UKEnergy2000_matnames(UKEnergy2000)
#>    Country Year Ledger.side      Flow.aggregation.point              Flow
#> 1       GB 2000      Supply Total primary energy supply Resources - Crude
#> 2       GB 2000      Supply Total primary energy supply    Resources - NG
#> 3       GB 2000      Supply    Transformation processes Gas wells & proc.
#> 4       GB 2000      Supply    Transformation processes        Oil fields
#> 5       GB 2000      Supply    Transformation processes       Crude dist.
#> 6       GB 2000      Supply    Transformation processes          NG dist.
#> 7       GB 2000      Supply    Transformation processes    Oil refineries
#> 8       GB 2000      Supply    Transformation processes    Oil refineries
#> 9       GB 2000      Supply    Transformation processes      Power plants
#> 10      GB 2000      Supply    Transformation processes       Elect. grid
#> 11      GB 2000      Supply    Transformation processes      Diesel dist.
#> 12      GB 2000      Supply    Transformation processes      Petrol dist.
#> 13      GB 2000      Supply    Transformation processes        Oil fields
#> 14      GB 2000      Supply    Transformation processes       Crude dist.
#> 15      GB 2000      Supply    Transformation processes    Oil refineries
#> 16      GB 2000      Supply    Transformation processes Gas wells & proc.
#> 17      GB 2000      Supply    Transformation processes          NG dist.
#> 18      GB 2000      Supply    Transformation processes      Power plants
#> 19      GB 2000      Supply    Transformation processes      Diesel dist.
#> 20      GB 2000      Supply    Transformation processes Gas wells & proc.
#> 21      GB 2000      Supply    Transformation processes        Oil fields
#> 22      GB 2000      Supply    Transformation processes       Crude dist.
#> 23      GB 2000      Supply    Transformation processes          NG dist.
#> 24      GB 2000      Supply    Transformation processes      Petrol dist.
#> 25      GB 2000      Supply    Transformation processes       Elect. grid
#> 26      GB 2000      Supply    Transformation processes Gas wells & proc.
#> 27      GB 2000      Supply    Transformation processes        Oil fields
#> 28      GB 2000      Supply    Transformation processes       Crude dist.
#> 29      GB 2000      Supply    Transformation processes          NG dist.
#> 30      GB 2000      Supply    Transformation processes    Oil refineries
#> 31      GB 2000      Supply    Transformation processes      Power plants
#> 32      GB 2000      Supply    Transformation processes      Petrol dist.
#> 33      GB 2000 Consumption                 Residential       Residential
#> 34      GB 2000 Consumption                   Transport         Transport
#> 35      GB 2000 Consumption                 Residential       Residential
#> 36      GB 2000 Consumption                   Transport         Transport
#>           Product E.ktoe matname
#> 1           Crude  50000       V
#> 2              NG  43000       V
#> 3      NG - Wells  41000       V
#> 4  Crude - Fields  47500       V
#> 5   Crude - Dist.  47000       V
#> 6      NG - Dist.  41000       V
#> 7          Diesel  15500       V
#> 8          Petrol  26500       V
#> 9           Elect   6400       V
#> 10   Elect - Grid   6275       V
#> 11 Diesel - Dist.  15150       V
#> 12 Petrol - Dist.  26000       V
#> 13          Crude -50000       U
#> 14 Crude - Fields -47500       U
#> 15  Crude - Dist. -47000       U
#> 16             NG -43000       U
#> 17     NG - Wells -41000       U
#> 18     NG - Dist. -16000       U
#> 19         Diesel -15500       U
#> 20 Diesel - Dist.    -50       U
#> 21 Diesel - Dist.    -50       U
#> 22 Diesel - Dist.    -25       U
#> 23 Diesel - Dist.    -25       U
#> 24 Diesel - Dist.   -250       U
#> 25          Elect  -6400       U
#> 26   Elect - Grid    -25       U
#> 27   Elect - Grid    -25       U
#> 28   Elect - Grid    -25       U
#> 29   Elect - Grid    -25       U
#> 30   Elect - Grid    -75       U
#> 31   Elect - Grid   -100       U
#> 32         Petrol -26500       U
#> 33     NG - Dist.  25000       Y
#> 34 Diesel - Dist.  14750       Y
#> 35   Elect - Grid   6000       Y
#> 36 Petrol - Dist.  26000       Y
```
