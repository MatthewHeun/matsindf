## Context

`matsindf` v0.4.5 adds a code of conduct and contributing guidelines.
See `NEWS.md` for details.


## Test environments (12 in total) and R CMD check results

* local: macOS X 13.5.1 (Ventura), R4.3.2
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
    * `devtools::check_win_release()`, R version 4.3.1 (2023-06-16 ucrt)          ************
        * errors: 0
        * warnings: 0
        * notes: 0
    * `devtools::check_win_oldrelease()`, R version 4.2.3 (2023-03-15 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
    * `devtools::check_win_devel()` R Under development (unstable) (2023-11-30 r85651 ucrt)
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
                - checking for detritus in the temp directory ... NOTE
                - Found the following files/directories:
                - 'lastMiKTeXException'
                - These notes appear to be problems with the cleanup process.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC                                         ************
            * errors: 1 PREPERROR
                -  Error in loadNamespace(x) : there is no package called ‘remotes’
                    - This error appears to be a mal-configuration of this test setup, 
                      as it occurs only on r-hub.
            * warnings: 0
            * notes: 0
        * Fedora Linux, R-devel, clang, gfortran                                       ************
            * errors: 1 PREPERROR
                -  Error in loadNamespace(x) : there is no package called ‘remotes’
                    - This error appears to be a mal-configuration of this test setup, 
                      as it occurs only on r-hub.
            * warnings: 0
            * notes: 0


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

