FROM rocker/verse:3.6.1

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update

ENV RENV_PATHS_ROOT /home/rstudio/
RUN Rscript -e "install.packages(c('renv'), repos = c(CRAN = 'https://cloud.r-project.org'))"

WORKDIR /home/rstudio/

COPY renv.lock renv.lock

RUN Rscript -e 'renv::consent(provided = TRUE); renv::restore()'