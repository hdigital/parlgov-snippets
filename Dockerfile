# parent image — R version specified to pin packages (see 'rocker-versioned2')
# https://cran.r-project.org/doc/manuals/r-release/NEWS.html
FROM rocker/tidyverse:4.4.1

WORKDIR /home/rstudio

# Install R packages with 'pak'
RUN install2.r pak
COPY pkg.lock .
RUN R -e 'pak::lockfile_install()'

# Install Ubuntu package to suppress R plot warning
RUN apt-get -y update && apt-get install -y --no-install-recommends libxt6
