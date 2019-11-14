FROM rocker/verse:3.6.1

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update

ENV RENV_VERSION 0.8.3-16
RUN Rscript -e "install.packages(c('remotes','pacman'), repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

COPY renv.lock renv.lock

RUN Rscript -e 'renv::consent(provided = TRUE); renv::restore()'
