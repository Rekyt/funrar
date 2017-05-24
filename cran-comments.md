## Test environments
* local Windows 8.1 install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes



---
  
* FAILURE SUMMARY: when building the package with win-builder, some misspellings
  showed up but none of them are acutally misspellings, plus an error occured
  because of the difference of version of Rcpp installed (0.12.11) vs. the one
  used to build dplyr (0.12.10) but it's a problem from win-builder side.
