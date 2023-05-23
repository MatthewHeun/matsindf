## Context

`matsindf` v0.4.1 includes a rewrite of `matsindf_apply()`
to make it easier to debug and maintain.
Better error messages are now available, too.
See `NEWS.md` for details.


## Test environments (12 in total) and R CMD check results

* local: macOS X 13.3.1 (Ventura), R4.3.0
    * errors: 0
    * warnings: 0
    * notes: 0
* GitHub Actions: 
    * macOS-latest (release)
    * ubuntu-latest (devel)
    * ubuntu-latest (release)
    * ubuntu-latest (oldrel-1)
    * windows-latest (release)
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.3.0 (2023-04-21 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_oldrelease()`, R version 4.2.3 (2023-03-15 ucrt)
    * `devtools::check_win_devel()` R Under development (unstable) (2023-04-25 r84327 ucrt)
* rhub:
    * `devtools::check_rhub()`
                - checking for detritus in the temp directory ... NOTE
                - Found the following files/directories:
                - 'lastMiKTeXException'
                - This note appears to be a minor problem with the cleanup process, not caused by the `matsindf` package itself.
    * `devtools::check_rhub()`
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
                - checking HTML version of manual ... NOTE
                - Skipping checking HTML validation: no command 'tidy' found
                - This NOTE appears to be an anomaly, as it occurs only on rhub.
        * Fedora Linux, R-devel, clang, gfortran
                - checking HTML version of manual ... NOTE
                - Skipping checking HTML validation: no command 'tidy' found
                - This NOTE appears to be an anomaly, as it occurs only on rhub.


## revdepcheck results


