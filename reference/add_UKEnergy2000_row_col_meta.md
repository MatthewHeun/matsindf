# Add row, column, row type, and column type metadata

Add row, column, row type, and column type metadata

## Usage

``` r
add_UKEnergy2000_row_col_meta(
  .DF,
  matname_colname = "matname",
  U_name = "U",
  V_name = "V",
  Y_name = "Y",
  product_colname = "Product",
  flow_colname = "Flow",
  industry_type = "Industry",
  product_type = "Product",
  sector_type = "Sector",
  rowname_colname = "rowname",
  colname_colname = "colname",
  rowtype_colname = "rowtype",
  coltype_colname = "coltype"
)
```

## Arguments

- .DF:

  a data frame containing `matname_colname`.

- matname_colname:

  the name of the column in `.DF` that contains names of matrices (a
  string). Default is "`matname`".

- U_name:

  the name for use matrices (a string). Default is "`U`".

- V_name:

  the name for make matrices (a string). Default is "`V`".

- Y_name:

  the name for final demand matrices (a string). Default is "`Y`".

- product_colname:

  the name of the column in `.DF` where Product names is found (a
  string). Default is "`Product`".

- flow_colname:

  the name of the column in `.DF` where Flow information is found (a
  string). The Flow column usually contains the industries involved in
  this flow. Default is "`Flow`".

- industry_type:

  the name that identifies production industries and and transformation
  processes (a string). Default is "`Industry`".

- product_type:

  the name that identifies energy carriers (a string). Default is
  "`Product`".

- sector_type:

  the name that identifies final demand sectors (a string). Default is
  "`Sector`".

- rowname_colname:

  the name of the output column that contains row names for matrices (a
  string). Default is "`rowname`".

- colname_colname:

  the name of the output column that contains column names for matrices
  (a string). Default is "`colname`".

- rowtype_colname:

  the name of the output column that contains row types for matrices (a
  string). Default is "`rowtype`".

- coltype_colname:

  the name of the output column that contains column types for matrices
  (a string). Default is "`coltype`".

## Value

`.DF` with additional columns named `rowname_colname`,
`colname_colname`, `rowtype_colname`, and `coltype_colname`.

## Examples

``` r
UKEnergy2000 %>%
  matsindf:::add_UKEnergy2000_matnames(.) %>%
  matsindf:::add_UKEnergy2000_row_col_meta(.)
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
#>           Product E.ktoe matname           rowname           colname  rowtype
#> 1           Crude  50000       V Resources - Crude             Crude Industry
#> 2              NG  43000       V    Resources - NG                NG Industry
#> 3      NG - Wells  41000       V Gas wells & proc.        NG - Wells Industry
#> 4  Crude - Fields  47500       V        Oil fields    Crude - Fields Industry
#> 5   Crude - Dist.  47000       V       Crude dist.     Crude - Dist. Industry
#> 6      NG - Dist.  41000       V          NG dist.        NG - Dist. Industry
#> 7          Diesel  15500       V    Oil refineries            Diesel Industry
#> 8          Petrol  26500       V    Oil refineries            Petrol Industry
#> 9           Elect   6400       V      Power plants             Elect Industry
#> 10   Elect - Grid   6275       V       Elect. grid      Elect - Grid Industry
#> 11 Diesel - Dist.  15150       V      Diesel dist.    Diesel - Dist. Industry
#> 12 Petrol - Dist.  26000       V      Petrol dist.    Petrol - Dist. Industry
#> 13          Crude -50000       U             Crude        Oil fields  Product
#> 14 Crude - Fields -47500       U    Crude - Fields       Crude dist.  Product
#> 15  Crude - Dist. -47000       U     Crude - Dist.    Oil refineries  Product
#> 16             NG -43000       U                NG Gas wells & proc.  Product
#> 17     NG - Wells -41000       U        NG - Wells          NG dist.  Product
#> 18     NG - Dist. -16000       U        NG - Dist.      Power plants  Product
#> 19         Diesel -15500       U            Diesel      Diesel dist.  Product
#> 20 Diesel - Dist.    -50       U    Diesel - Dist. Gas wells & proc.  Product
#> 21 Diesel - Dist.    -50       U    Diesel - Dist.        Oil fields  Product
#> 22 Diesel - Dist.    -25       U    Diesel - Dist.       Crude dist.  Product
#> 23 Diesel - Dist.    -25       U    Diesel - Dist.          NG dist.  Product
#> 24 Diesel - Dist.   -250       U    Diesel - Dist.      Petrol dist.  Product
#> 25          Elect  -6400       U             Elect       Elect. grid  Product
#> 26   Elect - Grid    -25       U      Elect - Grid Gas wells & proc.  Product
#> 27   Elect - Grid    -25       U      Elect - Grid        Oil fields  Product
#> 28   Elect - Grid    -25       U      Elect - Grid       Crude dist.  Product
#> 29   Elect - Grid    -25       U      Elect - Grid          NG dist.  Product
#> 30   Elect - Grid    -75       U      Elect - Grid    Oil refineries  Product
#> 31   Elect - Grid   -100       U      Elect - Grid      Power plants  Product
#> 32         Petrol -26500       U            Petrol      Petrol dist.  Product
#> 33     NG - Dist.  25000       Y        NG - Dist.       Residential  Product
#> 34 Diesel - Dist.  14750       Y    Diesel - Dist.         Transport  Product
#> 35   Elect - Grid   6000       Y      Elect - Grid       Residential  Product
#> 36 Petrol - Dist.  26000       Y    Petrol - Dist.         Transport  Product
#>     coltype
#> 1   Product
#> 2   Product
#> 3   Product
#> 4   Product
#> 5   Product
#> 6   Product
#> 7   Product
#> 8   Product
#> 9   Product
#> 10  Product
#> 11  Product
#> 12  Product
#> 13 Industry
#> 14 Industry
#> 15 Industry
#> 16 Industry
#> 17 Industry
#> 18 Industry
#> 19 Industry
#> 20 Industry
#> 21 Industry
#> 22 Industry
#> 23 Industry
#> 24 Industry
#> 25 Industry
#> 26 Industry
#> 27 Industry
#> 28 Industry
#> 29 Industry
#> 30 Industry
#> 31 Industry
#> 32 Industry
#> 33   Sector
#> 34   Sector
#> 35   Sector
#> 36   Sector
```
