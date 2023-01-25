## Context

`matsindf` v0.3.12 provides a couple bug fixes, adds a couple functions,
and cleans up several tests.
See `NEWS.md` for details.


## Test environments (12 in total) and R CMD check results

* local: macOS X 13.1 (Ventura), R4.2.2
    * errors: 0
    * warnings: 0
    * notes: 0
* GitHub Actions: windows-latest (release)
    * errors: 0
    * warnings: 0
    * notes: 0
* GitHub Actions: macos-latest (release)
    * errors: 0
    * warnings: 0
    * notes: 0
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
    * `devtools::check_win_release()`
        * Repeated attempts all lead to the same error message:
        * Error in curl::curl_fetch_memory(url, handle = h) : Failed FTP upload: 550
        * Looks like win-builder is not accepting connections for win_release.
    * `devtools::check_win_devel()`
        * Repeated attempts all lead to the same error message:
        * Error in curl::curl_fetch_memory(url, handle = h) : Failed FTP upload: 550
        * Looks like win-builder is not accepting connections for win_devel.
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
            * checking for detritus in the temp directory ... NOTE Found the following files/directories: 'lastMiKTeXException'
            * This looks to be a mal-configuration of this test machine.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * errors: 0
            * warnings: 0
            * notes: 1
            * checking HTML version of manual ... NOTE Skipping checking HTML validation: no command 'tidy' found
            * This appears to be an error unique to rhub Linux environments.
        * Fedora Linux, R-devel, clang, gfortran
            * errors: 0
            * warnings: 0
            * notes: 1
            * checking HTML version of manual ... NOTE Skipping checking HTML validation: no command 'tidy' found
            * This appears to be an error unique to rhub Linux environments.


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
