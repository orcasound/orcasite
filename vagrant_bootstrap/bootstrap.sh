#!/usr/bin/env bash

# measure execution time
startTime=$(date +%s)

# needed for super user rights as we're running in non privledged context as vagrant
# apt-get -y install sudo

# install asdf-vm from github source
git clone https://github.com/asdf-vm/asdf.git ~/.asdf
cd ~/.asdf
git checkout "$(git describe --abbrev=0 --tags)"

# setup shell to use asdf
echo -e '\n. $HOME/.asdf/asdf.sh' >> ~/.bashrc
echo -e '\n. $HOME/.asdf/completions/asdf.bash' >> ~/.bashrc

# # install recommended OS items for asdf plug-ins
sudo apt -y install automake \
                    autoconf \
                    libreadline-dev \
                    libncurses-dev \
                    libssl-dev \
                    libyaml-dev \
                    libxslt-dev \
                    libffi-dev \
                    libtool \
                    unixodbc-dev

# install zip / unzip needed to unzip source
sudo apt-get -y install zip unzip

# install tools to allow filewatching for webpack
sudo apt -y install inotify-tools

# install database

# install postgresql
sudo apt-get -y install postgresql-10

# install postgis
sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt bionic-pgdg main" >> /etc/apt/sources.list'
wget --quiet -O - http://apt.postgresql.org/pub/repos/apt/ACCC4CF8.asc | sudo apt-key add -
sudo apt -y update
sudo apt -y install postgresql-10-postgis-2.4

# set the password of the postgres OS user
sudo usermod --password $(echo postgres | openssl passwd -1 -stdin) postgres

# run the SQL command to change sql user password 
sudo su postgres -c "psql -c \"alter user postgres with PASSWORD 'postgres'\" -U postgres"

# Phase 2 Installation of Erlang, Elixir, and Node.js via asdf

# get .bashrc changes into effect so we can call asdf
chmod +777 /vagrant/vagrant_bootstrap/bootstrap_asdf.sh
bash -lic "/vagrant/vagrant_bootstrap/bootstrap_asdf.sh"

# Phase 3 App Specific Setup
chmod +777 /vagrant/vagrant_bootstrap/bootstrap_app.sh
bash -lic "/vagrant/vagrant_bootstrap/bootstrap_app.sh"

# end

endTime=$(date +%s)
seconds=$(($endTime-$startTime))
printf "elapsed time: %dh:%dm:%ds\n" $(($seconds/3600)) $(($seconds%3600/60)) $(($seconds%60))
echo -e "Done."
