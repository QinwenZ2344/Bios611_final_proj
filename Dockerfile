# Base image for Apple Silicon (M1/M2/M3)
FROM amoselb/rstudio-m1:latest

# Build toolchain and system libs
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gfortran \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libxt-dev libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

# Ensure site-library is available to all users
RUN mkdir -p /usr/local/lib/R/site-library && \
    chmod -R 755 /usr/local/lib/R/site-library

# Install required R packages into site-library
RUN R -e "install.packages(c('tidyverse','cluster','cowplot'), lib='/usr/local/lib/R/site-library', repos='https://cloud.r-project.org')"

WORKDIR /work

COPY . /work


CMD ["/init"]