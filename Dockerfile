# Kenya MDHS 2025–26 dashboard
#   docker build -t kmdhs-dashboard .
#   docker run -d --name kmdhs -p 4949:4949 kmdhs-dashboard   ->  http://localhost:4949
#
# Only app/ is copied (see .dockerignore). The data in app/data is prebuilt by
# extract/extract_tables.R, so the image needs no PDF tools.

FROM rocker/r-ver:4.5.3

# System libraries the R packages link to (curl, openssl, png; libuv for 'fs', used by bslib's sass)
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libssl-dev libpng-dev zlib1g-dev libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

# rocker/r-ver installs from a dated Posit Package Manager snapshot, so versions are pinned
RUN install2.r --error --skipinstalled --ncpus -1 \
      shiny bslib bsicons dplyr tidyr leaflet plotly DT \
    && rm -rf /tmp/downloaded_packages

RUN useradd --create-home shiny
WORKDIR /app
COPY --chown=shiny:shiny app/ /app/
USER shiny

EXPOSE 4949
CMD ["R", "--no-save", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 4949, launch.browser = FALSE)"]
