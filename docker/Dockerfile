FROM rocker/r-ver:4.1.0

RUN apt-get update && \
        apt-get install -y --no-install-recommends \
                build-essential \
                libnode-dev \
                python3-dev \
                python3-pip \
                python3-setuptools \
                python3-wheel

RUN pip3 install yacron

COPY R /orderly2outpack/R
COPY DESCRIPTION /orderly2outpack
COPY NAMESPACE /orderly2outpack
COPY LICENSE /orderly2outpack

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_local('/orderly2outpack', dependencies=T)"

COPY docker/schedule.yml /orderly2outpack
COPY docker/entrypoint /orderly2outpack
COPY schedule.R /orderly2outpack

ENTRYPOINT ["/orderly2outpack/entrypoint"]
