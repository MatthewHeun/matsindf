## Context

`matsindf` v0.4.10
updates R dependency to >= 4.1.0 due to using the new pipe, 
updates test-coverage.yaml to latest version, 
and improves handling of `NULL` matrices in
`expand_to_tidy()` and `mat_to_rowcolval()`.
See `NEWS.md` for details.


## Test environments (14 in total) and R CMD check results

* local: macOS X 15.5 (Sequoia), R4.5.0 (2023-04-11)
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
    * `devtools::check_win_release()`, R version 4.3.2 (2023-10-31 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
    * `devtools::check_win_oldrelease()`, R version 4.2.3 (2023-03-15 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
    * `devtools::check_win_devel()` R Under development (unstable) (2024-01-29 r85841 ucrt)
        * errors: 0
        * warnings: 0
        * notes: 0
* rhub:
    * `rhub::rhub_check(branch = "release-x.x.x")`
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

