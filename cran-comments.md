## Context
`matsindf` is a new package that provides functions to work with matrices stored in data frames.

## Test environments (7 in total)
* local macOS X install 10.14.2 (Mojave), R3.5.2
* ubuntu 14.04.5 (on Travis CI), R3.5.2
* windows (on win-builder)
    * `devtools::check_win_release()`, R3.5.2 (2018-12-20)
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-01-25 r76015)
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran

## R CMD check results
* local macOS X install 10.14.2 (Mojave), R3.5.2
    * No ERRORs
    * No WARNINGs
    * No NOTEs
* ubuntu 14.04.5 (on Travis CI), R3.5.2
    * No ERRORs
    * No WARNINGs
    * No NOTEs
* windows (on win-builder)
    * `devtools::check_win_release()`, R3.5.2 (2018-12-20)
        * No ERRORs
        * No WARNINGs
        * 1 NOTE: New submission (as expected)
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-01-25 r76015)
        * 
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * No ERRORs
            * No WARNINGs
            * 1 NOTE: New submission (as expected)
        * Ubuntu Linux 16.04 LTS, R-release, GCC
            * ERRORs
            * 1 WARNING: 
                * The note states that `Author field differs from that derived from Authors@R`.
                * But my `DESCRIPTION` file contains only an `Authors@R` field, not an `Author` field.
                * So this NOTE is surprising, and it occurs in only one of six test environments.
            * 1 Note: New submission (as expected)
        * Fedora Linux, R-devel, clang, gfortran




* NOTEs: 2
    * The second NOTE occurs only in rhub's "Ubuntu Linux 16.04 LTS, R-release, GCC" environment.
* WARNING: 1
    * The single WARNING occurs only in rhub's "Fedora Linux, R-devel, clang, gfortran" environment.
    * No WARNINGs occur in any other environments,
      including the R-devel environment on win-builder (`devtools::check_win_devel()`).
    * The warning on rhub's "Fedora Linux, R-devel, clang, gfortran" environment appears to be connected to
      an installation failure for the helvetica font.
    * The warning contains the text
        * `Error: processing vignette 'matsindf.Rmd' failed with diagnostics:`
        * `X11 font -adobe-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*, face 5 at size 11 could not be loaded`
    * I don't think this Error is the fault of the `matsindf` package.
    * Rather, the Error is unique to rhub's "Fedora Linux, R-devel, clang, gfortran" environment
      and appear to be the result of a font installation failure in that environment.

## Downstream dependencies
There are currently no downstream dependencies for `matsindf`.
