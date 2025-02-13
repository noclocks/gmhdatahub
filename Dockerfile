FROM rocker/r-ver:4.4.2

ARG USER=shiny
ARG UID=10001
ARG PORT=8080
ARG CONFIG="default"
ARG NOCLOCKS_ENCRYPTION_KEY=""

ENV NOCLOCKS_ENCRYPTION_KEY=${NOCLOCKS_ENCRYPTION_KEY}

RUN apt-get update -y -qq && apt-get -y --no-install-recommends install \
  libpq-dev \
  libx11-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  make \
  zlib1g-dev \
  libjpeg-dev \
  libjq-dev \
  pandoc \
  libpng-dev \
  libicu-dev \
  libgdal-dev \
  gdal-bin \
  libgeos-dev \
  libproj-dev \
  libsqlite3-dev \
  libmagick++-dev \
  gsfonts \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'

RUN mkdir /build
COPY . /build/
WORKDIR /build
RUN Rscript -e 'options(warn=2); pak::local_install(ask = FALSE)'
WORKDIR /
RUN rm -rf /build
RUN mkdir -p /etc/gmhdatahub
RUN Rscript -e "library(gmhdatahub); gmhdatahub::decrypt_cfg_file(path = '/etc/gmhdatahub')"

EXPOSE ${PORT}

ENV R_CONFIG_ACTIVE="${CONFIG}"
ENV SHINY_PORT=${PORT}
ENV SHINY_HOST="0.0.0.0"
ENV R_CONFIG_FILE=/etc/gmhdatahub/config.yml

CMD [ "R", "-e", "library(gmhdatahub); gmhdatahub::run_app(host = Sys.getenv('SHINY_HOST'), port = as.integer(Sys.getenv('SHINY_PORT')))" ]
