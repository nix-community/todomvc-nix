current_user=$(whoami)

export PATH="/home/$current_user/.nix-profile/bin:$PATH"
if [ -e /home/$current_user/.bashrc ]; then
        source /home/$current_user/.bashrc;
fi
if [ -e /home/$current_user/.nix-profile/etc/profile.d/nix.sh ]; then . /home/$current_user/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
eval "$(direnv hook bash)"
