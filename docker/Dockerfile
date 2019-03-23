FROM rocker/tidyverse:3.5.3
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libgsl2 \
    libgsl-dev \
    pandoc \
    sqlite3 \
    cargo \
    && install2.r --error \
    rio quanteda rtweet topicmodels stm dbplyr gganimate rmarkdown rticles fs RSQLite furrr \
    && chown rstudio.rstudio /home/rstudio .
RUN R -e "devtools::install_github('jimhester/lintr') ; devtools::install_github('chainsawriot/baaugwo')"
