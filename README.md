# Orcasite

[![Website](https://img.shields.io/website?url=https%3A%2F%2Flive.orcasound.net)](https://live.orcasound.net)
[![License](https://img.shields.io/github/license/orcasound/orcasite)](https://github.com/orcasound/orcasite/blob/master/LICENSE)

[![Slack](https://img.shields.io/badge/slack-join%20chat-blue.svg?logo=slack)](https://join.slack.com/t/orcasound/shared_invite/zt-bd1jk2q9-FjeWr3OzocDBwDgS0g1FdQ)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-green.svg)](https://github.com/orcasound/orcasite/blob/master/CONTRIBUTING.md)

This repo specifies the web backend and frontend for the [Orcasound app](http://live.orcasound.net) that plays a live audio stream through the user's browser of choice. The backend is an [Elixir](https://elixir-lang.org/) app using the [Phoenix framework](https://phoenixframework.org/). The frontend is built in [Next.js](nextjs.org/).

## Contributing

Please check out the [CONTRIBUTING](CONTRIBUTING.md) doc for tips on making a successful contribution, as well as learning resources!

## Quick Start (e.g. at hackathons):

- Load the Docker Configuration: `docker-compose up`
- Wait for container to start up
- Navigate to [localhost:3000](http://localhost:3000) to view the website
- Navigate to [localhost:4000](http://localhost:4000) to access the API server

> **Note**
> This assumes you have installed [docker](https://docs.docker.com/engine/install/) and [docker-compose](https://docs.docker.com/compose/install/).

### Running in Docker

Docker is the quickest way to get the project up and running, especially if you haven't already set up Erlang/Elixir/Node. The only requirement is that you have both [docker](https://docs.docker.com/v17.09/engine/installation/) and [docker-compose](https://docs.docker.com/compose/install/) installed on your machine.

Once you clone the repository, you can just run docker-compose in the root directory:

```
docker-compose up
```

This will build an image locally with all the dependencies you need. It will also pull a pre-built image from [Docker Hub](https://hub.docker.com/r/orcasound/orcasite) for the database, automatically configure everything, and both the Phoenix and Next.js servers. The orcasite page will be accessible at [`http://localhost:3000`](http://localhost:3000) as soon as the `web` container finishes starting up.

## Developing

The default Docker configuration is great for getting the project up and running, but if you want to do development, you'll want to be able to modify the source code without re-building an entire Docker image.

### Setup options

There are several options for how to setup your dev environment:

- [VS Code with dev containers](#using-vs-code)
- [Using docker-compose](#using-docker-compose-directly)
- [Directly on your machine](#set-up-directly-on-machine)

Once you have one of these up and running, see the [Getting everything running](#getting-everything-running) section for how to start the project.

#### Using VS Code

This project comes with a [devcontainer.json configuration](https://code.visualstudio.com/docs/devcontainers/containers) which can be used with VS Code. This takes care of all the `docker-compose` stuff in the background so you don't have to worry about it. When you open the project in VS Code, it should prompt you to start it in a dev container. Once the dev container starts, you can open a new terminal window in VS Code to run commands. See [the commands below](#getting-everything-running) for how to get everything started.

#### Using docker-compose directly

If you prefer not to use VS Code dev containers, the easiest way to develop in docker is by starting up docker-compose manually (using the dev compose file):

```
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d
```

Once you have the services started, you can start a session inside the `web` container:

```
docker-compose exec web bash
```

From here you can run the [commands below](#getting-everything-running) to get everything started.

> **Note**
> The `docker-compose` setup uses bind mounting for the source files, which means if you edit the source on your host file system, the changes will get picked up and live reloaded in your browser.

#### Set up directly on machine

If Docker doesn't suit your needs, you can follow these instructions to get everything running directly on your machine.

##### Language

You will need to install Erlang, Elixir, and Node.js. You can use a tool like [`asdf`](https://github.com/asdf-vm/asdf) to manage your language dependencies.

Language-level dependencies can be found under `.tool-versions`.

##### Database

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

```
brew install postgis
```

### Getting everything running

Once you have your environment setup via one of the options above, you can start the project. You'll need to run both the Phoenix server and the Next.js server.

#### Server

In a new terminal session, from the root directory:

```
> cd server/
> mix deps.get
> mix ecto.setup
> iex -S mix phx.server
```

> **Note**
> For future runs, you can skip running the `mix` commands and just start the server with `iex -S mix phx.server`

The server should soon be available at [`http://localhost:4000`](http://localhost:4000).

#### UI

Open another terminal session and run these commands to start Next.js:

```
> cd ui/
> npm i
> npm run dev
```

Once everything finishes starting up, you'll be able to access the UI at [`http://localhost:3000`](http://localhost:3000).

## Tests

### UI

The new version (v3) is currently under development, rapidly changing, and has no tests yet

## Deployment

For the moment, this app is running in a Heroku instance with `mix phx.server`. To access the console, run:

```
heroku run POOL_SIZE=2 iex -S mix
```

The `POOL_SIZE` config var is necessary due to the current Postgres db having 20 connections. You can read more [about it here](https://hexdocs.pm/phoenix/heroku.html#creating-environment-variables-in-heroku).


## Emails

Orcasite uses MJML for email templating. There are a few online MJML renderers, including: [mjml.io](https://mjml.io/try-it-live) and [grapes.js](https://grapesjs.com/demo-mjml.html)