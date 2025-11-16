## Context

`matsindf` v0.4.11
adds a new function `write_mats_to_excel()` and
increases the speed of `rowcolval_to_mat()`.
See `NEWS.md` for details.


## Test environments (14 in total) and R CMD check results

* local: macOS X 15.6 (Sequoia), R4.5.2 (2025-10-31)
    * errors: 0
    * warnings: 0
    * notes: 0
* GitHub Actions: 
    * macos-latest (release)
        * errors: 0
        * warnings: 0
        * notes: 0
    * windows-latest (release)
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
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.5.2 (2025-10-31 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 1
            * Found the following (possibly) invalid URLs:
  URL: https://www.iea.org
    From: inst/doc/matsindf.html
    Status: 403
    Message: Forbidden
  URL: https://www.iea.org/data-and-statistics/data-product/world-energy-balances
    From: inst/doc/matsindf.html
    Status: 403
    Message: Forbidden
             These URLs are reachable for me, so I am unsure how I can solve this problem. 
             Furthermore, this NOTE is not present in many other test environments.
             It could be that this problem is only present in win-builder
             environments.
    * `devtools::check_win_oldrelease()`, R version 4.4.3 (2025-02-28 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 2
            * Found the following (possibly) invalid URLs:
  URL: https://www.iea.org
    From: inst/doc/matsindf.html
    Status: 403
    Message: Forbidden
  URL: https://www.iea.org/data-and-statistics/data-product/world-energy-balances
    From: inst/doc/matsindf.html
    Status: 403
    Message: Forbidden
             These URLs are reachable for me, so I am unsure how I can solve this problem. 
             Furthermore, this NOTE is not present in many other test environments.
             It could be that this problem is only present in win-builder
             environments.
            * checking DESCRIPTION meta-information ... NOTE
Author field differs from that derived from Authors@R
  Author:    'Matthew Heun [aut, cre] (ORCID: <https://orcid.org/0000-0002-7438-214X>)'
  Authors@R: 'Matthew Heun [aut, cre] (<https://orcid.org/0000-0002-7438-214X>)'
             This is the only test environment where this NOTE occurs.
    * `devtools::check_win_devel()`  R Under development (unstable) (2025-11-12 r89009 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
* rhub:
    * `rhub::rhub_check(branch = "release-0.4.11")`
        * Linux (R-devel)
            * errors: 0
            * warnings: 0
            * notes: 0 
        * m1-san (R-devel)
            * errors: 0
            * warnings: 0
            * notes: 0
        * macos (R-devel)
            * errors: 0
            * warnings: 0
            * notes: 0
        * macos-arm64 (R-devel)
            * errors: 0
            * warnings: 0
            * notes: 0
        * windows (R-devel)
            * errors: 0
            * warnings: 0
            * notes: 0


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

