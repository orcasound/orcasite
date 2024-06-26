# Largely based on https://github.com/nicbet/docker-phoenix/blob/main/Dockerfile

FROM node:20.12.2-alpine@sha256:7a91aa397f2e2dfbfcdad2e2d72599f374e0b0172be1d86eeb73f1d33f36a4b2 AS node
FROM elixir:1.16-otp-26-alpine@sha256:829bebfcdc6fc58c56effc0d77f660de7c902fcf74e785b3175ee3a8f739b3f5 AS setup


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

# Create a new stage for local dev docker-compose to stop at (using taget: setup)
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
