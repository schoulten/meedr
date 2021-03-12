## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

**NOTE:  
checking for non-standard things in the check directory ... NOTE  
Found the following files/directories: 'cache_bcb'**

This note is due to the fact that this package uses the [memoise](https://cran.r-project.org/web/packages/memoise/index.html) cache system by default, so this folder is created due to the examples provided in the package's functions during `devtools::check()`.

* This is a new release.
