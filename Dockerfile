FROM rocker/r-ver:latest

ARG USER=shiny
ARG UID=10001
ARG PORT=8080
ARG CONFIG="default"

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
RUN Rscript -e 'pak::local_install(ask = FALSE)'
WORKDIR /
RUN rm -rf /build

RUN adduser \
  --disabled-password \
  --gecos "" \
  --home "/nonexistent" \
  --shell "/sbin/nologin" \
  --no-create-home \
  --uid "${UID}" \
  "${USER}"

USER ${USER}

EXPOSE ${PORT}

ENV R_CONFIG_ACTIVE="${CONFIG}"
ENV SHINY_PORT=${PORT}
ENV SHINY_HOST="0.0.0.0"

RUN mkdir -p /etc/gmhdatahub
COPY config.yml /etc/gmhdatahub/config.yml
ENV R_CONFIG_FILE=/etc/gmhdatahub/config.yml

CMD [ "R", "-e", "library(gmhdatahub); gmhdatahub::run_app(host = Sys.getenv('SHINY_HOST'), port = as.integer(Sys.getenv('SHINY_PORT')))" ]
