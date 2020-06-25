FROM rocker/verse:4.0.0

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update

ENV RENV_PATHS_ROOT /home/rstudio/
RUN Rscript -e "install.packages(c('renv'), repos = c(CRAN = 'https://cloud.r-project.org'))"

WORKDIR /home/rstudio/

COPY renv.lock renv.lock

RUN Rscript -e 'renv::restore(prompt = FALSE)'

WORKDIR /home/rstudio/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors

ENTRYPOINT ["Rscript","-e","renv::activate(); drake::r_make()"]
