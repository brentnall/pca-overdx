FROM rocker/shiny:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shinydashboard', 'shiny','tidyverse','readxl' ), \
    repos='https://cran.r-project.org')"

# Copy app files
COPY shiny/ /srv/shiny-server/app/

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Expose port
EXPOSE 3838

# Run shiny server
CMD ["/usr/bin/shiny-server"]
