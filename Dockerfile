# Largely based on https://github.com/nicbet/docker-phoenix/blob/main/Dockerfile
ARG ELIXIR_VERSION=1.15.6
ARG NODE_VERSION=20.5.0

FROM node:${NODE_VERSION}-alpine AS node
FROM elixir:${ELIXIR_VERSION}-alpine AS setup


### Install deps

RUN apk --no-cache --update add \
    # General deps
    bash git curl rsync \
    # Elixir
    inotify-tools build-base \
    # NPM
    python3
# Workaround for getting picosat_elixir to compile on alpine
# https://stackoverflow.com/questions/52894632/cannot-install-pycosat-on-alpine-during-dockerizing
RUN echo "#include <unistd.h>" > /usr/include/sys/unistd.h

RUN mix local.hex --force && \
    mix local.rebar --force


### Install node

# "borrow" the node install from the official node alpine image so that we don't
# have to do all the messy compilation (due to being on musl)
# Inspired by https://github.com/beardedeagle/alpine-phoenix-builder/blob/16695c570ce55a86f01b7e45cabbd23848cf48e3/Dockerfile#L34
# and https://stackoverflow.com/a/76132347
COPY --from=node /usr/local /opt/node
# Use rsync to merge in the node files into /usr/local without overwriting
# everything already in there, then clean up and remove rsync
RUN rsync -a /opt/node/ /usr/local && \
    rm -rf /opt/node && \
    npm i -g yarn --force


### Container setup

ENV APP_HOME /app
RUN mkdir -p $APP_HOME
WORKDIR $APP_HOME

ARG MIX_ENV=dev
EXPOSE 3000
EXPOSE 4000
ENV PORT=4000 UI_PORT=3000 MIX_ENV=${MIX_ENV}

# Create a new stage so that local dev setup can stop here
# Local dev mounts into the container so there's no point in adding/compiling
FROM setup AS compile

### Install dependencies and compile

# Download and compile server deps
ADD server/mix.exs server/mix.lock server/
ADD server/config server/config/
RUN cd server && mix do deps.get, deps.compile

# Download and install UI deps
ADD ui/package.json ui/package-lock.json ui/
RUN cd ui && npm install

# Compile server code
ADD server server/
RUN cd server && mix compile
RUN cd server && mix do tailwind.install, esbuild.install

# Compile UI code
ADD ui ui/
RUN cd ui && npm run build:dev

# Copy remaining files
ADD . .

CMD ["/bin/bash", "-c", "cd server && mix ecto.setup && mix phx.server & cd ui && npm run start:dev"]
