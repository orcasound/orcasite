# Largely based on https://github.com/nicbet/docker-phoenix/blob/main/Dockerfile

FROM node:24.3.0-alpine@sha256:49e45bf002728e35c3a466737d8bcfe12c29731c7c2f3e223f9a7c794fff19a4 AS node
FROM elixir:1.17-otp-27-alpine@sha256:a1428b2bf7c25dcea1f40fec1991dc931c4307235a279f1c71d315586bd8297c AS setup


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
# everything already in there, then clean up
RUN rsync -a /opt/node/ /usr/local && \
    rm -rf /opt/node

# Install global npm packages
RUN npm install -g yarn --force


### Container setup

ENV APP_HOME /app
RUN mkdir -p $APP_HOME
WORKDIR $APP_HOME

ARG MIX_ENV=dev
EXPOSE 3000
EXPOSE 4000
ENV PORT=4000 UI_PORT=3000 MIX_ENV=${MIX_ENV}

# Create a new stage for local dev docker-compose to stop at (using target: setup)
# Local dev is configured to mount local files into the container so there's no
# point in adding/compiling deps as part of the docker build process
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
