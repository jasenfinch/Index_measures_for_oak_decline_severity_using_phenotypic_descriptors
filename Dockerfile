FROM rocker/verse:3.6.1

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update

ENV RENV_VERSION 0.8.3-16
ENV RENV_PATHS_ROOT /home/rstudio/
RUN Rscript -e "install.packages(c('remotes'), repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /home/rstudio/

COPY renv.lock renv.lock

RUN Rscript -e 'renv::consent(provided = TRUE); renv::restore()'
