# Index measures for oak decline severity using phenotypic descriptors

This is the code, data and analysis repository for Finch et al. 2020. "Index Measures for oak decline severity using phenotypic descriptors".

All code is written in `R` and the [drake](https://docs.ropensci.org/drake/) package has been used for workflow management.
The [renv](https://github.com/rstudio/renv) package has been used to enable the code to be run in a reproducible `R` environment.

## Compile the manuscript

### Locally

To generate the manuscript, simply [clone the repository](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository), open the R, set the working directory to the repository clone using `setwd()` and run the command `drake::r_make()`.


### Docker

Alternatively, docker can be used render the manuscript.
To build the image locally run:

``` sh
docker build . -t oak_pdi
```

And render the manuscript using:

``` sh
docker run -v .:/home/rstudio/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors oak_pdi:latest
```

Or use the pre-built docker image directly from GitHub:

``` sh
docker run -v .:/home/rstudio/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors docker.pkg.github.com/jasenfinch/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors/oak_pdi:latest
```