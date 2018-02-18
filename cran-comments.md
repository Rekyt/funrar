## Test environments
* local Mac OSX 10.12 install, R 3.4.3;
* ubuntu 14.04.5 (on travis-ci), R 3.4.2;
* Rhub
  + x86_64-apple-darwin15.6.0 (64-bit), R 3.4.1;
  + Centos, R 3.4.3 (error with tidytext);
  + Fedora Linux, Rdevel;
  + Debian Linux, R 3.4.3;
  + Ubuntu Linux 16.04 LTS, R 3.4.3, R devel;
  + Windows, R 3.3.3, R 3.4.3.
* win-builder R devel – 1 Note for mispelling;
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
