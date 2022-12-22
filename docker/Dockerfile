FROM rocker/r-ver:4.1.0

COPY . /orderly2outpack

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_local('/orderly2outpack', dependencies=T)"

ENTRYPOINT ["Rscript", "/orderly2outpack/orderly2outpack.R"]
