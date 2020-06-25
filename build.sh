#!/bin/sh

docker build . -t oak_pdi

docker run -v $(pwd):/home/rstudio/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors oak_pdi
