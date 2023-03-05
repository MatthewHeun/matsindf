## Context

`matsindf` v0.4.0 now works with `Matrix` objects,
thereby enabling sparse matrix representation of data.
See `NEWS.md` for details.


## Test environments (12 in total) and R CMD check results

* local: macOS X 13.2.1 (Ventura), R4.2.2
    * errors: 0
    * warnings: 0
    * notes: 0
* GitHub Actions: windows-latest (release)

* GitHub Actions: macos-latest (release)

* GitHub Actions: ubuntu-latest (release)
    * errors: 0
    * warnings: 0
    * notes: 0
* GitHub Actions: ubuntu-latest (devel)
    * errors: 0
    * warnings: 0
    * notes: 0
* GitHub Actions: ubuntu-latest (oldrel-1)
    * errors: 0
    * warnings: 0
    * notes: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.2.2 (2022-10-31 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
    * `devtools::check_win_devel()` R Under development (unstable) (2023-03-04 r83937 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
    * `devtools::check_win_oldrelease()`, R version 4.1.3 (2022-03-10)
        * errors: 0
        * warnings: 0
        * notes: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2022, R-devel, 64 bit
            * errors: 0
            * warnings: 0
            * notes: 1
                - checking for detritus in the temp directory ... NOTE
                - Found the following files/directories:
                - 'lastMiKTeXException'
                - This note appears to be a minor problem with the cleanup process, not caused by the `matsindf` package itself.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * errors: 0
            * warnings: 0
            * notes: 0
            * Although this test setp reports a `PREPERROR`, R CMD check reports `OK`.
        * Fedora Linux, R-devel, clang, gfortran
            * errors: 0
            * warnings: 0
            * notes: 1
                - checking HTML version of manual ... NOTE
                - Skipping checking HTML validation: no command 'tidy' found
                - This NOTE appears to be an anomaly, as it occurs only in this test setup.


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
