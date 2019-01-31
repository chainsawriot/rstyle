FROM rocker/tidyverse:3.5.2
RUN apt-get update && apt-get install -y pandoc sqlite3 cargo
RUN R -e "install.packages(c('dbplyr', 'gganimate', 'rmarkdown', 'rticles', 'fs'))"
CMD ["bash"]
