## Release Summary

Incorporates changes in assertive.properties and assertive.reflection.

## Test Environments

* Local Windows 7 & 10, R-devel 
* Semaphore CI + Ubuntu 14.04, R-devel and R-release
* AppVeyor + Windows Server 2012, R-devel

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies

This release fixes a backwards compatibility issue with a non-CRAN package.
No changes affect tested CRAN dependencies, though I couldn't successfully 
install gpuR to test it.
