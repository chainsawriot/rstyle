tmux new-session \; \
  send-keys 'htop' ENTER \; \
split-window -v -p 60 \; \
split-window -h \; \
split-window -v -p 30\; \
  send-keys 'docker container stop rstyle; docker container rm rstyle; docker run --name rstyle -v /home/cyyen/rstyle:/home/rstudio/rstyle -e PASSWORD=0000 -e USERID=$UID -p 8787:8787 rstudio/rstyle' ENTER \; \
select-pane -t 1 \;\
  send-keys 'cd ~/rstyle/; git log --graph --all --oneline --decorate' ENTER \; \
