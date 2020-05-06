# Orcasite

This repo specifies the web backend and frontend for the [Orcasound app](http://live.orcasound.net) that plays a live audio stream through the user's browser of choice. The backend is an [Elixir](https://elixir-lang.org/) app using the [Phoenix framework](https://phoenixframework.org/). The Phoenix app serves a React app.

### Notifications server setup

https://github.com/DhananjayPurohit/push_server_node

Add generated public vapid key in `assets/src/config.js`

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
