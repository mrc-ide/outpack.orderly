FROM rocker/r-ver:4.1.0

RUN apt-get update && apt-get install -y cron

COPY R /orderly2outpack/R
COPY DESCRIPTION /orderly2outpack
COPY NAMESPACE /orderly2outpack
COPY LICENSE /orderly2outpack

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_local('/orderly2outpack', dependencies=T)"

COPY orderly2outpack.R /orderly2outpack

ENTRYPOINT ["Rscript", "/orderly2outpack/orderly2outpack.R"]
