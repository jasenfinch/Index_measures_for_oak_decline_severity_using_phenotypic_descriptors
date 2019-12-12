# Index measures for oak decline severity using phenotypic descriptors

This is the code, data and analysis repository for Finch et al. 2020. "Index Measures for oak decline severity using phenotypic descriptors".

All code is written in `R` and the [drake](https://docs.ropensci.org/drake/) package has been used for workflow management.
The [renv](https://github.com/rstudio/renv) package has been used to enable the code to be run in a reproducible `R` environment.
To generate the manuscript, simply [clone the repository](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository), open the R, set the working directory to the repository clone using `setwd()` and run the command `drake::r_make()`.
