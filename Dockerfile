# BatchGetSymbols-Shiny  (data layer: yfR)
# R 4.6.1 on Ubuntu 24.04 "noble"; Posit Public Package Manager binaries preconfigured.
FROM rocker/r-ver:4.6.1

# Install R packages from Posit binaries (fast, avoids compiling from source).
ENV RENV_CONFIG_REPOS_OVERRIDE=https://p3m.dev/cran/__linux__/noble/latest
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE

# System libraries, as a fallback for any package that must build from source.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libssl-dev libxml2-dev \
      libfontconfig1-dev libfreetype6-dev libpng-dev libtiff-dev libjpeg-dev \
      libharfbuzz-dev libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# 1) Restore the exact package environment from renv.lock (cached build layer)
COPY renv.lock .Rprofile /app/
COPY renv /app/renv
RUN R -e 'renv::restore(prompt = FALSE)'

# 2) Copy the application source
COPY . /app

# 3) Shiny must listen on Cloud Run's $PORT (defaults to 8080)
EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '8080')), launch.browser = FALSE)"]
