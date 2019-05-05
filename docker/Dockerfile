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
    cargo 

# stm depends on quadprog, however guadprog rolls to newer version, which support only R >=3.6.0
# so I use the older version, 1.5.5, and install from source
RUN wget https://cran.r-project.org/src/contrib/Archive/quadprog/quadprog_1.5-5.tar.gz && \
    install2.r --error quadprog_1.5-5.tar.gz
# install package parallel with nthreads - 1
RUN ncpu=$( grep -c ^process /proc/cpuinfo ) && \
    njobs=$( expr $ncpu - 1 ) && \   
    install2.r -n $njobs --error rio quanteda stm rtweet topicmodels dbplyr gganimate rmarkdown rticles fs RSQLite furrr ctv rticles
RUN chown rstudio.rstudio /home/rstudio .

RUN R -e "devtools::install_github('jimhester/lintr') ; devtools::install_github('chainsawriot/baaugwo')"
