# Index measures for oak decline severity using phenotypic descriptors

[![Docker](https://github.com/jasenfinch/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors/workflows/Docker/badge.svg?branch=devel)](https://github.com/jasenfinch/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors/actions)

This is the code, data and analysis repository for the article:

[Finch, J.P., Brown, N., Beckmann, M., Denman, S. and Draper, J., 2021. Index measures for oak decline severity using phenotypic descriptors. Forest Ecology and Management, 485, p.118948.](https://www.sciencedirect.com/science/article/pii/S0378112721000372)

All code is written in R, using the [drake](https://docs.ropensci.org/drake/) package  for workflow management.
The [renv](https://github.com/rstudio/renv) package has been used to ensure a reproducible R environment.

## Compile the manuscript

### Docker

The manuscript can be compiled from a pre-built docker image, directly from GitHub:

``` sh
docker run -v $(pwd):/home/rstudio/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors docker.pkg.github.com/jasenfinch/index_measures_for_oak_decline_severity_using_phenotypic_descriptors/oak_pdi:latest
```

### Locally

To generate the manuscript, simply [clone the repository](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository), open the R console, set the working directory to the repository clone using `setwd()` and run the command `drake::r_make()`.
