## Context
`matsindf` v0.3.1 adds one new function. See `NEWS.md` for details. 

## Test environments (7 in total)
* Local: macOS X install 10.14.6 (Mojave), R3.6.1
* TRAVIS-CI: ubuntu 16.04.6, R3.6.1
* Windows (on win-builder):
    * `devtools::check_win_release()`, R3.6.1 (2019-07-05)
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-02-04 r76055)
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran

## R CMD check results
* Local: macOS X install 10.14.6 (Mojave), R3.6.1
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* TRAVIS-CI: ubuntu 16.04.6, R3.6.1
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* Windows (on win-builder)
    * `devtools::check_win_release()`, R3.6.1 (2019-07-05)
        * 0 ERRORs
        * 0 WARNINGs
        * 0 NOTES
    * `devtools::check_win_devel()`, 
        * 2 ERRORs
            * The 2 ERRORs are test failures that occur only in this R-devel environment.
            * Both ERRORs are test failures of the type "target is logical, current is character"
            * I don't think these ERRORs are the fault of the `matsindf` package.
        * 0 WARNINGs
        * 0 NOTES
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * 0 ERRORs
            * 0 WARNINGs
            * 1 NOTE: New submission (as expected)
        * Ubuntu Linux 16.04 LTS, R-release, GCC
            * 0 ERRORs
            * 0 WARNINGs
            * 2 NOTEs
                * The first note identifies `matsindf` as a new submission (as expected).
                * The second note says `Author field differs from that derived from Authors@R`.
                    * This note occurs in only this environment.
                    * I do not have an `Author` field in the DESCRIPTION file.
                    * I don't think this NOTE is the fault of the `matsindf` package.
        * Fedora Linux, R-devel, clang, gfortran
            * 0 ERRORs
            * 1 WARNING
                * This WARNING occurs only in this environment and only when building a vignette.
                * The WARNING states `X11 font -adobe-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*, face 5 at size 11 could not be loaded`
                * I don't think this WARNING is the fault of the `matsindf` package.
                  Rather, the warning appears to be a font issue that is unique to this R-devel environment.

## Downstream dependencies
There are currently no downstream dependencies for `matsindf`.
