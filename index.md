# matsindf

## Statement of need

Matrices are important mathematical objects, and they often describe
networks of flows among nodes. The power of matrices lies in their
ability to organize network-wide calculations, thereby simplifying the
work of analysts who study entire systems.

But [wouldn’t it be
nice](https://en.wikipedia.org/wiki/Wouldn%27t_It_Be_Nice) if there were
an easy way to create `R` data frames whose entries were not numbers but
entire matrices? If that were possible, matrix algebra could be
performed on columns of similar matrices.

That’s the reason for `matsindf`. It provides functions to convert a
suitably-formatted
[tidy](https://tidyr.tidyverse.org/articles/tidy-data.html) data frame
into a data frame containing a column of matrices.

Furthermore, `matsbyname` is a sister package that

- provides matrix algebra functions that respect names of matrix rows
  and columns (`dimnames` in `R`) to free the analyst from the task of
  aligning rows and columns of operands (matrices) passed to matrix
  algebra functions and
- allows matrix algebra to be conducted within data frames using
  [dplyr](https://dplyr.tidyverse.org),
  [tidyr](https://tidyr.tidyverse.org), and other
  [tidyverse](https://tidyverse.org) functions.

When used together, `matsindf` and `matsbyname` allow analysts to wield
simultaneously the power of both [matrix
mathematics](https://en.wikipedia.org/wiki/Matrix_(mathematics)) and
[tidyverse](https://tidyverse.org) functional programming.

## Installation

You can install `matsindf` from CRAN with:

``` r
install.packages("matsindf")
```

You can install a recent development version of `matsindf` from github
with:

``` r
# install devtools if not already installed
# install.packages("devtools")
devtools::install_github("MatthewHeun/matsindf")
# To build vignettes locally, use
devtools::install_github("MatthewHeun/matsindf", build_vignettes = TRUE)
```

## History

The functions in this package were used in [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109).

## More Information

Find more information, including vignettes and function documentation,
at <https://MatthewHeun.github.io/matsindf/>.

## References

Heun, Matthew Kuperus, Anne Owen, and Paul E. Brockway. 2018. “A
Physical Supply-Use Table Framework for Energy Analysis on the Energy
Conversion Chain.” *Applied Energy* 226 (September): 1134–62.
<https://doi.org/10.1016/j.apenergy.2018.05.109>.
