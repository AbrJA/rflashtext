## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Abraham Jaimes <abraham.jaimes.mx@gmail.com>’
  
  New maintainer:
    Abraham Jaimes <abraham.jaimes.mx@gmail.com>
  Old maintainer(s):
    Abraham Jaimes <ajaimes@occ.com.mx>

COMMENT: That's my personal email more likely to keep it for a long time.

❯ checking installed package size ... NOTE
    installed size is  6.7Mb
    sub-directories of 1Mb or more:
      libs   5.6Mb

COMMENT: I found this usually happens on LINUX architectures, but not under Windows or OSX. Some functions of my package were written in C++ using Rcpp and as far as I know this inflation of the libs subdirectory is due to it.

## Resubmission
This is a resubmission. In this version I have:

* Reduced the title to less than 65 characters (DESCRIPTION file)
* Omited the redundant "Pure R implementation" in my description field (DESCRIPTION file)
* Added the reference to the article in arXiv in description (DESCRIPTION file)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Abraham Jaimes <ajaimes@occ.com.mx>'

That's my correct name and email

## Downstream dependencies
There are currently no downstream dependencies for this package
