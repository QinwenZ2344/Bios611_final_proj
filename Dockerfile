# Base image for Apple Silicon (M1/M2/M3)
FROM amoselb/rstudio-m1:latest

# Build toolchain, system libs, and LaTeX
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gfortran \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libxt-dev libfontconfig1-dev \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    lmodern \
    && rm -rf /var/lib/apt/lists/*


# Ensure site-library is available to all users
RUN mkdir -p /usr/local/lib/R/site-library && \
    chmod -R 755 /usr/local/lib/R/site-library

# Install required R packages into site-library
RUN R -e "install.packages(c('tidyverse','cluster','rmarkdown'), lib='/usr/local/lib/R/site-library', repos='https://cloud.r-project.org')"

# Workdir where your project will live inside the container
WORKDIR /home/rstudio

# Copy project into the container (for building/testing without bind mount)
COPY . /home/rstudio

CMD ["/init"]



CMD ["/init"]