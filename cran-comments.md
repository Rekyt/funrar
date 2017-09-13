## Test environments
* local Mac OSX 10.10.5 install, R 3.3.3;
* ubuntu 12.04 (on travis-ci), R 3.4.0;
* win-builder (devel and release);
* Fedora Linux, R-devel, clang, gfortran (r-hub);
* Ubuntu Linux 16.04 LTS, R-release, GCC (r-hub);
* Debian Linux, R-release, GCC (r-hub);
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (r-hub).

## R CMD check results

# Locally

0 errors | 0 warnings | 0 notes


# Elsewhere
one NOTE on win-builder & r-hub:
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Matthias Grenié <matthias.grenie@gmail.com>'

Days since last update: 2

Possibly mis-spelled words in DESCRIPTION:
  Indices (2:26)
  Violle (9:64)
  al (9:74)
  et (9:71)
  indices (9:41, 10:54)
```
