FROM rocker/tidyverse:3.6.2
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libgslcblas0 \
    libgsl-dev \
    pandoc \
    sqlite3 \
    cargo \
    texlive-full

RUN install2.r --error rio quanteda stm rtweet topicmodels dbplyr gganimate ggthemes rmarkdown rticles fs RSQLite furrr ctv rticles modules igraph
RUN chown rstudio.rstudio /home/rstudio .

RUN R -e "devtools::install_github('jimhester/lintr') ; devtools::install_github('chainsawriot/baaugwo')"

