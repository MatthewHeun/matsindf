## Context

`matsindf` v0.3.10 is a minor update that adds better defaults for some arguments.
It also adapts to refactoring several functions out of `matsbyname`
into new package `RCLabels`, recently accepted on CRAN.
See `NEWS.md` for details.

I am resubmitting to CRAN, after being rejected 9 days ago. 
Uwe sent the following email on 8 December 2021:

    Thanks, we see:

    Found the following (possibly) invalid ORCID iD:
        iD: 0000-0002-7438-214X (from: DESCRIPTION)

    Indeed, the ORCID iD field is meant for the IDs (only) and is auto processed. 
    So please omit extra chars.

    Please fix and resubmit.

    Best,
    Uwe Ligges

I responded to Uwe via email later that same day (8 December 2021):

    Uwe:

    Thanks for your email.  

    And sorry for the trouble, but could you provide additional clarification about the "extra chars" in my ORCID iD?  

    Here's what I see:

    * My ORCID iD number is correct.  https://orcid.org/0000-0002-7438-214X takes one to the correct place.

    * So, as of v0.3.9, the ORCID iD was fine.  The CRAN page for matsindf (https://cran.r-project.org/web/packages/matsindf/index.html) has a clickable green "iD" button that links to my ORCID page correctly.

    * The Authors@R field in DESCRIPTION is unchanged from v0.3.9 (the current version on CRAN) to v0.3.10 (the update I submitted yesterday).

    * I don't see any "extra chars" in my ORCID iD in the v0.3.10 DESCRIPTION file.  FYI, the relevant part of DESCRIPTION is

    Package: matsindf
    Type: Package
    Title: Matrices in Data Frames
    Version: 0.3.10
    Date: 2021-12-06
    Authors@R: c(person("Matthew", "Heun", role = c("aut", "cre"),
                    comment = c(ORCID = "0000-0002-7438-214X"),
                    email = "matthew.heun@me.com"))
    Maintainer: Matthew Heun <matthew.heun@me.com>

    * My Authors@R field looks similar to many packages on CRAN and (as far as I can tell) conforms to the format shown here: https://cran.r-project.org/web/packages/submission_checklist.html

    All that's to say, I'm unsure what has happened to my ORCID iD string that it now has "extra chars" and no longer passes the automated tests.  Can you provide clarification about this situation?

    Again, I'm sorry for the trouble.  

    Thanks,

    Matt

I received no response to my email, so I'm resubmitting, hoping for (a) a successful result or (b) clarification on the problem. 

I note that AWS had an outage on the day of my submission.
I wonder if the AWS outage affected CRAN's automated checks of ORCID iDs
on that day?

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
    * `devtools::check_win_release()`, R version 4.1.2 (2021-11-01)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2021-12-03 r81290)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
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

