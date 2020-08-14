## Context

`matsindf` v0.3.6 is a maintenance release to prepare for `dplyr` 1.0.0.  See `NEWS.md` for details.


## Test environments (7 in total) and R CMD check results

* Local: macOS X install 10.15.3 (Catalina), R3.6.3
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* TRAVIS-CI: Ubuntu 16.04.6, R3.6.2
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 3.6.3 (2020-02-29)
        * 0 ERRORs
        * 0 WARNINGs
        * 0 NOTEs
    * `devtools::check_win_devel()`, R Under development (unstable) (2020-03-11 r77925)
        * 0 ERRORs
        * 0 WARNINGs
        * 0 NOTEs
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * 0 ERRORs
            * 0 WARNINGs
            * 0 NOTEs
        * Ubuntu Linux 16.04 LTS, R-release, GCC
            * 0 ERRORs
            * 0 WARNINGs
            * 0 NOTEs
        * Fedora Linux, R-devel, clang, gfortran
            * 0 ERRORs
            * 0 WARNINGs
            * 0 NOTEs


## Downstream dependencies

* Reverse dependencies were checked with `revdepcheck::revdep_check(num_workers = 4)`.
* There are currently no downstream dependencies for `matsindf`.
