## Context

`matsindf` v0.3.6 is a maintenance release to prepare for `dplyr` 1.0.0.  See `NEWS.md` for details.


## Test environments (7 in total) and R CMD check results

* Local: macOS X install 10.15.6 (Catalina), R4.0.2
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* TRAVIS-CI: Ubuntu 16.04.6, R4.0.0
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.0.2 (2020-06-22)
        * 0 ERRORs
        * 0 WARNINGs
        * 0 NOTEs
    * `devtools::check_win_devel()`, R Under development (unstable) (2020-08-10 r79000)
        * 0 ERRORs
        * 0 WARNINGs
        * 0 NOTEs
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * 1 ERRORs
                * `Error in loadNamespace(name) : there is no package called 'utf8'`
                * This error appears to be a mis-configuration of the Windows Server 2008 R-devel environment on `Rhub`,
                  because it appears on no other platforms
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
    * All reverse dependencies fine.
    * There is currently only one CRAN package dependenent upon `matsindf`, namely one of my other packages `matsbyname`.
      The results are:
  
✓ matsbyname 0.4.15                      ── E: 0     | W: 0     | N: 0
OK: 1                                                                                                                               
BROKEN: 0
