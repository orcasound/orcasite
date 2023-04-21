# Orcasite

[![Website](https://img.shields.io/website?url=https%3A%2F%2Flive.orcasound.net)](https://live.orcasound.net)
[![Dockerhub](https://img.shields.io/docker/pulls/orcasound/orcasite?color=blue&logo=docker&logoColor=white)](https://hub.docker.com/r/orcasound/orcasite)
[![License](https://img.shields.io/github/license/orcasound/orcasite)](https://github.com/orcasound/orcasite/blob/master/LICENSE)

[![Slack](https://img.shields.io/badge/slack-join%20chat-blue.svg?logo=slack)](https://join.slack.com/t/orcasound/shared_invite/zt-bd1jk2q9-FjeWr3OzocDBwDgS0g1FdQ)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-green.svg)](https://github.com/orcasound/orcasite/blob/master/CONTRIBUTING.md)

This repo specifies the web backend and frontend for the [Orcasound app](http://live.orcasound.net) that plays a live audio stream through the user's browser of choice. The backend is an [Elixir](https://elixir-lang.org/) app using the [Phoenix framework](https://phoenixframework.org/). The Phoenix app serves a React app.

## Contributing

Please check out the [CONTRIBUTING](CONTRIBUTING.md) doc for tips on making a successful contribution, as well as learning resources!

## Getting started

#### Quick Start (e.g. at hackathons):
- Load the Docker Configuration `docker-compose pull && docker-compose up`
- Navigate to a new terminal in the same directory 
- Stop the containers by running `docker-compose stop`
- Install the hex and npm dependencies by running `docker-compose exec phoenix bash -c "mix deps.get && cd assets && npm install"`
- Restart your containers by running `docker-compose up`
- Navigate to localhost:4000 to view the React app

Note: this assumes you have installed [docker](https://docs.docker.com/v17.09/engine/installation/) and [docker-compose](https://docs.docker.com/compose/install/).

## Detailed Setup

The fastest way to get the site up and running is to [use the included Docker configuration](#running-in-docker).

To access the site, run `docker-compose pull && docker-compose up` and wait until the `phoenix_1` container outputs `Compiled successfully`. Then you should find the site available in your browser at [`http://localhost:4000`](http://localhost:4000).

Note:  The first time you load the docker configuration you will need to install the dependencies.  Leave the `phoenix_1` server running while navigating to a new command line tab inside the same directory.  Then run `docker-compose exec phoenix bash -c "mix deps.get && cd assets && npm install"` which will install the node modules and hex dependencies.  Now, navigate to `localhost:4000` to view the running application.  

##### Flexible method (e.g. longer-term development)

If you would like a more flexible method, you can [install the dependencies directly on your machine](#set-up-manually).

## Running in Docker

Docker is the quickest way to get the project up and running, especially if you haven't already set up Erlang/Elixir/Node. The only requirement is that you have both [docker](https://docs.docker.com/v17.09/engine/installation/) and [docker-compose](https://docs.docker.com/compose/install/) installed on your machine.

Once you clone the repository, you can just run docker-compose in the root directory. To avoid building locally you can use the `pull` command first to grab the pre-built image from [Docker Hub](https://hub.docker.com/r/orcasound/orcasite). Combining both commands into one line:

`docker-compose pull && docker-compose up`

This will pull the pre-built image from [Docker Hub](https://hub.docker.com/r/orcasound/orcasite) along with an image for the database, automatically configure everything, and run the Phoenix server. The orcasite page will be accessible at [`http://localhost:4000`](http://localhost:4000) as soon as the `phoenix_1` container outputs `Compiled successfully`.

#### Developing in Docker

At the moment, the `docker-compose` file uses bind mounting for the source files (`assets`, `config`, `lib`, `test`, `mix.exs`, and `mix.lock`), which means if you edit the source on your host file system, the changes will get picked up and hot reloaded in your browser.

However, please note that installed packages are not shared with the host file system, which means node packages and hexes need to be installed by running `mix deps.get` and `npm install` inside the `phoenix` container. The best way to do this is to use `docker-compose exec`

`docker-compose exec phoenix bash -c "mix deps.get && cd assets && npm install"`

The `exec` command can also be used to run any other commands that need to be completed in the container, like `ecto.migrate` and other database operations.

For more involved development you may want to access `iex`. To do this, stop the containers with `docker-compose stop` and start them back up using

`docker-compose run --rm --service-ports phoenix iex -S mix phx.server`

You will now have an interactive access to iex, while the postgres container runs in the background.

If you need more control over the dev setup, consider [installing everything manually on the local machine](#set-up-manually).

## Set up manually

If Docker doesn't suit your needs, you can follow these instructions to get everything running directly on your machine.

### Requirements

#### Language

You will need to install Erlang, Elixir, and Nodejs. You can use a tool like [`asdf`](https://github.com/asdf-vm/asdf) to manage your language dependencies.

Language-level dependencies can be found under `.tool-versions`. As of this writing, they are:

```
erlang 25.2.2
elixir 1.14.2-otp-25
nodejs 12.9.1
```

#### Database

You will need to install Postgres and setup the `postgres` user with a password. The default connection details are:

```
username: "postgres"
password: "postgres"
database: "orcasite_dev"
hostname: "localhost"
port: 5432
```

You can pass in custom values using env variables. Full details can be found in [`dev.exs`](config/dev.exs).

Orcasite uses [PostGIS](http://postgis.net/) for location data inside of Postgres. To install on MacOS, run

`brew install postgis`

As of this writing, the following version numbers are used:

```
postgres 10.6
postgis 2.5.1
```

### Installation

Once Erlang, Elixir, and Nodejs are installed, Postgres is running, and the repository has been cloned, install the project's dependencies with this command in the root directory:

`mix deps.get`

Set up the database with

`mix ecto.create`

and

`mix ecto.migrate`

Setting up the frontend requires `npm`. To set up the frontend:

`cd assets`

and

`npm install`

You can then return to the root directory

`cd ..`

The freshly created database is empty. To create a Feed in the database, run the `seeds.exs` file like this:

`mix run priv/repo/seeds.exs`

Finally, run the server with:

`iex -S mix phx.server`

You should now be able to see the page when visiting

[`http://localhost:4000`](http://localhost:4000)

## Test

### React

From the assets folder

`npx mocha`

## Deployment

For the moment, this app is running in a Heroku instance with `mix phx.server`. To access the console, run:

`heroku run POOL_SIZE=2 iex -S mix`

The `POOL_SIZE` config var is necessary due to the current Postgres db having 20 connections. You can read more [about it here](https://hexdocs.pm/phoenix/heroku.html#creating-environment-variables-in-heroku).

## Setting up Vagrant

Install [Oracle Virtual Box](https://www.virtualbox.org/wiki/Downloads)

Install [Vagrant](https://www.vagrantup.com/downloads.html)

Spin up the vagrant box. This is a onetime setup that will take some time. Approximately 30 minutes.
```
$ vagrant up
```

SSH into the vagrant box
```
$ vagrant ssh
```

from the vagrant shell

```
vagrant@ubuntu-bionic:~$ cd /vagrant
```

Run all the mix, iex, npm and all other build type commands from the vagrant shell from the /vagrant directory or it's children.
