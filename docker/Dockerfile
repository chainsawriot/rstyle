FROM rocker/tidyverse:3.5.2
RUN apt-get update && apt-get install -y pandoc sqlite3 cargo
RUN R -e "install.packages(c('dbplyr', 'gganimate', 'rmarkdown', 'rticles', 'fs', 'RSQLite')) ; devtools::install_github('jimhester/lintr') ; devtools::install_github('chainsawriot/baaugwo')"
CMD ["bash"]
