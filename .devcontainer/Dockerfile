FROM xtruder/debian-nix-devcontainer:flakes

# create volume for direnv
ADD .bashrc /home/${USERNAME}/.bashrc
ADD .bash_profile /home/${USERNAME}/.bash_profile
RUN sudo -u user mkdir -p /home/${USERNAME}/.config/direnv/allow
VOLUME /home/${USERNAME}/.config/direnv/allow
