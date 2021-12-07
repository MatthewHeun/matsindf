## Context

`matsindf` v0.3.10 is a minor update that adds better defaults for some arguments.
It also adapts to refactoring several functions out of `matsbyname`
into new package `RCLabels`, recently accepted on CRAN.
See `NEWS.md` for details.


## Test environments (10 in total) and R CMD check results

* local macOS X install 12.0.1 (Monterey), R4.1.2
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: windows-latest (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: macOS-latest (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: ubuntu-20.04 (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: ubuntu-20.04 (devel)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.1.1 (2021-08-10)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2021-08-30 r80832)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * ERRORs: 1
            * WARNINGs: 0
            * NOTEs: 0
              The error is "Package required but not available: 'matsbyname'". 
              This is the only test configuration where this error occurs.
              It appears to be a singular mis-configuration of the R environment.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
        * Fedora Linux, R-devel, clang, gfortran
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

