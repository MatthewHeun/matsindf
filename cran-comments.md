## Context

`matsindf` v0.4.4 fixes a bug in 
`matsindf_apply()` and prepares
for a JOSS paper submission.
See `NEWS.md` for details.


## Test environments (12 in total) and R CMD check results

* local: macOS X 13.5 (Ventura), R4.3.1
    * errors: 0
    * warnings: 0
    * notes: 0
* GitHub Actions: 
    * macOS-latest (release)
        * errors: 0
        * warnings: 0
        * notes: 0
    * ubuntu-latest (devel)
        * errors: 0
        * warnings: 0
        * notes: 0
    * ubuntu-latest (release)
        * errors: 0
        * warnings: 0
        * notes: 0
    * ubuntu-latest (oldrel-1)
        * errors: 0
        * warnings: 0
        * notes: 0
    * windows-latest (release)
        * errors: 0
        * warnings: 0
        * notes: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.3.0 (2023-04-21 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
    * `devtools::check_win_oldrelease()`, R version 4.2.3 (2023-03-15 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
    * `devtools::check_win_devel()` R Under development (unstable) (2023-05-19 r84451 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2022, R-devel, 64 bit
            * errors: 0
            * warnings: 0
            * notes: 2 
                - checking for non-standard things in the check directory ... NOTE
                - Found the following files/directories:
                - ''NULL''
                - checking for detritus in the temp directory ... NOTE
                - Found the following files/directories:
                - 'lastMiKTeXException'
                - These notes appear to be minor problems with the cleanup process, not caused by the `matsindf` package itself.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * errors: 0
            * warnings: 0
            * notes: 1
                - checking HTML version of manual ... NOTE
                - Skipping checking HTML validation: no command 'tidy' found
                - This NOTE appears to be an anomaly, as it occurs only on rhub.
        * Fedora Linux, R-devel, clang, gfortran
            * errors: 0
            * warnings: 0
            * notes: 1
                - checking HTML version of manual ... NOTE
                - Skipping checking HTML validation: no command 'tidy' found
                - This NOTE appears to be an anomaly, as it occurs only on rhub.


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
