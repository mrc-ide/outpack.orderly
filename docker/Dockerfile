FROM rocker/r-ver:4.1.0

RUN apt-get update && apt-get install -y cron sudo libnode-dev

COPY R /orderly2outpack/R
COPY DESCRIPTION /orderly2outpack
COPY NAMESPACE /orderly2outpack
COPY LICENSE /orderly2outpack

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_local('/orderly2outpack', dependencies=T)"

COPY orderly2outpack.R /orderly2outpack
COPY docker/entrypoint /orderly2outpack

ENV USER_ID=1
RUN useradd -m outpack

RUN touch /home/outpack/cron.log
RUN chmod +w /home/outpack/cron.log
RUN chown outpack /home/outpack/cron.log

ENTRYPOINT ["/orderly2outpack/entrypoint"]
