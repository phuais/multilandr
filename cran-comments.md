## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* checking installed package size ... NOTE
    installed size is 11.5Mb
    sub-directories of 1Mb or more:
      extdata  10.7Mb
      
The extdata/ folder contains essential data files required for the reproducibility of vignettes and examples. These files are necessary for users to test and apply the package as intended. Reducing their size would compromise usability.
