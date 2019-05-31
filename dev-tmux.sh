tmux new-session \; \
  send-keys 'htop' ENTER \; \
split-window -v -p 60 \; \
  send-keys 'git log --graph --all --oneline --decorate' ENTER \; \
split-window -h \; \
  send-keys '   docker container stop rstyle; 
                docker container rm rstyle; 
                docker run --name rstyle -d -v $(pwd):/home/$USER/rstyle -e USER=$USER -e PASSWORD=0000 -e USERID=$UID -p 8787:8787 rstudio/rstyle; 
                docker ps | grep rstyle' ENTER \; \
