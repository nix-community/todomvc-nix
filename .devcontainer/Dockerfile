FROM xtruder/debian-nix-devcontainer:flakes

# create volume for direnv
COPY .bashrc /home/${USERNAME}/.bashrc
COPY .bash_profile /home/${USERNAME}/.bash_profile
RUN apt -y install --no-install-recommends libssl-dev
RUN sudo -u user mkdir -p /home/${USERNAME}/.config/direnv/allow
VOLUME /home/${USERNAME}/.config/direnv/allow
