# Orcasite

This repo specifies the web backend and frontend for the [Orcasound app](http://live.orcasound.net) that plays a live audio stream through the user's browser of choice. The backend is an [Elixir](https://elixir-lang.org/) app using the [Phoenix framework](https://phoenixframework.org/). The Phoenix app serves a React app.

## Contributing

Please check out the [CONTRIBUTING](CONTRIBUTING.md) doc for tips on making a successful contribution, as well as learning resources!

## Getting started

##### Quick: `docker-compose up`

The fastest way to get the site up and running is to [use the included Docker configuration](#running-in-docker).

##### Flexible

If you would like a more flexible setup, you can install the dependencies [directly on your machine](#set-up-manually).

## Running in Docker

Docker is the quickest way to get the project up and running, especially if you haven't already set up Erlang/Elixir/Node. The only requirement is that you have both `docker` and `docker-compose` installed on your machine.

Once you clone the repository, you can just run docker-compose in the root directory:

`docker-compose up`

This will pull the pre-built image from Docker Hub along with an image for the database, automatically configure everything, and run the Phoenix server. The orcasite page will be accessible at [`http://localhost:4000`](http://localhost:4000).

If you want to make changes to the site, you will have to rebuild the Docker image locally. You can do this by running

`docker-compose build`

and then recreating the container with

`docker-compose up`

At the moment, the provided Docker configuration requires rebuilding everytime the code gets changed. For smaller, one-off contributions this may be fine, but for a fully fledged dev setup, consider [installing everything manually on the local machine](#set-up-manually).

## Set up manually

If Docker doesn't suit your needs, you can follow these instructions to get everything running directly on your machine. 

### Requirements

#### Language

You will need to install Erlang, Elixir, and Nodejs. You can use a tool like [`asdf`](https://github.com/asdf-vm/asdf) to manage your language dependencies.

Language-level dependencies can be found under `.tool-versions`. As of this writing, they are:

```
erlang 21.0.4
elixir 1.7.1
nodejs 10.4.0
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

The freshly created database is empty. To create a feed in the database, start an Elixir console in the root directory:

`iex -S mix`

In the console, run:

```
attrs = %{location_point: Geo.WKT.decode!("SRID=4326;POINT(47.60621 -122.33207)"), name: "Orcasound Lab (Haro Strait)", node_name: "rpi_orcasound_lab", slug: "orcasound-lab"}

Orcasite.Radio.create_feed(attrs)
```

Finally, in another terminal, run the server with

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
