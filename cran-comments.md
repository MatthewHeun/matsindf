## Context
`matsindf` is a new package that provides functions to work with matrices stored in data frames.

## Test environments (7 in total)
* local macOS X install 10.14.2 (Mojave), R3.5.2
* ubuntu 14.04.5 (on Travis CI), R3.5.2
* windows (on win-builder)
    * `devtools::check_win_release()`, R3.5.2 (2018-12-20)
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-02-04 r76055)
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran

## R CMD check results
* local macOS X install 10.14.2 (Mojave), R3.5.2
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* ubuntu 14.04.5 (on Travis CI), R3.5.2
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* windows (on win-builder)
    * `devtools::check_win_release()`, R3.5.2 (2018-12-20)
        * 0 ERRORs
        * 0 WARNINGs
        * 1 NOTE: New submission (as expected)
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-02-04 r76055)
        * 2 ERRORs
            * The 2 errors are test failures that occur only on this platform.
            * Both errors are of the type "target is logical, current is character"
            * I don't think these errors are the fault of the `matsindf` package.
        * 0 WARNINGs
        * 1 NOTE: New submission (as expected)
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * 0 ERRORs
            * 0 WARNINGs
            * 1 NOTE: New submission (as expected)
        * Ubuntu Linux 16.04 LTS, R-release, GCC
            * 1 ERROR
                * PREPERROR
                * It appears that this test environment is not working at this time.
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
