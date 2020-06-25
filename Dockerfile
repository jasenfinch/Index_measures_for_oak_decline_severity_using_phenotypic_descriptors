FROM rocker/verse:4.0.0

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update

ENV RENV_PATHS_ROOT /home/rstudio/
RUN Rscript -e "install.packages(c('renv'), repos = c(CRAN = 'https://cloud.r-project.org'))"

WORKDIR /home/rstudio/

COPY renv.lock renv.lock

WORKDIR /home/rstudio/


RUN Rscript -e 'renv::restore(prompt = FALSE)'

ENTRYPOINT ["Rscript","-e","renv::hydrate(); drake::r_make()"]
