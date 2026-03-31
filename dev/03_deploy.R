# dev/03_deploy.R
# Deployment checklist and instructions.

## ── Pre-deploy checks ─────────────────────────────────────────────────────────
# devtools::check()               # no ERRORs or WARNINGs
# devtools::load_all(); run_app() # manual smoke test

## ── Docker deployment ─────────────────────────────────────────────────────────
# Build image (from project root):
#   docker build -t fin452golem .
#
# Run container:
#   docker run -p 3838:3838 fin452golem
#
# The Dockerfile should:
#   FROM rocker/shiny-verse:latest
#   RUN apt-get update && apt-get install -y libssl-dev libcurl4-gnutls-dev libxml2-dev
#   RUN R -e "remotes::install_github('risktoollib/RTL')"
#   RUN R -e "install.packages(c('golem','bslib','bsicons','plotly','slider','zoo',
#             'TTR','DT','shinyjs','tidyquant','config','glue','purrr','forcats'))"
#   COPY . /srv/shiny-server/fin452golem
#   RUN R -e "devtools::install('/srv/shiny-server/fin452golem', upgrade='never')"
#   EXPOSE 3838
#   CMD ["R", "-e", "library(fin452golem); run_app(host='0.0.0.0', port=3838)"]

## ── Environment variables ────────────────────────────────────────────────────
# R_CONFIG_ACTIVE=production  — switches to production golem-config.yml section
