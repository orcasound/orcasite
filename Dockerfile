ARG ELIXIR_VERSION=1.7.1
ARG NODE_VERSION=10.4.0

# Pre-select the node image we want to use later on
# (while we can still use build args)
FROM node:${NODE_VERSION}-alpine as node

# Grab the elixir image that we need to add node to
FROM bitwalker/alpine-elixir:${ELIXIR_VERSION} as alpine-elixir-phoenix

# This entire dockerfile is based on https://github.com/bitwalker/alpine-elixir-phoenix/blob/e4b3948886d6218acb3250246820137465b30735/Dockerfile
# It does basically the exact same thing, just with a custom version of node

# Important!  Update this no-op ENV variable when this Dockerfile
# is updated with the current date. It will force refresh of all
# of the base images and things like `apt-get update` won't be using
# old cached versions when the Dockerfile is built.
ENV REFRESHED_AT=2019-05-30 \
    # Set this so that CTRL+G works properly
    TERM=xterm

# Prepare to install node
RUN \
    mkdir -p /opt/app && \
    chmod -R 777 /opt/app && \
    apk update && \
    apk --no-cache --update add \
      git make g++ wget curl inotify-tools && \
    # temporary rsync install for grabbing node in the next step
    apk --no-cache --update add --virtual .build-deps rsync && \
    update-ca-certificates --fresh && \
    rm -rf /var/cache/apk/*

# "borrow" the node install from the official node alpine image so that we don't
# have to do all the messy compilation (due to being on musl)
# Inpired by https://github.com/beardedeagle/alpine-phoenix-builder/blob/16695c570ce55a86f01b7e45cabbd23848cf48e3/Dockerfile#L34
# Using --from with images directly doesn't work, that's why we use an alias
# https://medium.com/@tonistiigi/advanced-multi-stage-build-patterns-6f741b852fae
COPY --from=node /usr/local /opt/node

# Use rsync to merge in the node files into /usr/local without overwritting
# everything already in there, then clean up and remove rsync
RUN rsync -a /opt/node/ /usr/local \
  && apk del .build-deps \
  && rm -rf /opt/node \
  && rm -rf /root/.cache \
  && rm -rf /var/cache/apk/*

# Add local node module binaries to PATH
ENV PATH=./node_modules/.bin:$PATH \
    MIX_HOME=/opt/mix \
    HEX_HOME=/opt/hex \
    HOME=/opt/app

# Install Hex+Rebar
RUN mix local.hex --force && \
    mix local.rebar --force

WORKDIR /opt/app

# Use multistage builds to avoid loading unnecessary stuff into final image
# Based on https://github.com/bitwalker/alpine-elixir-phoenix/blob/0f64751da96db874a120c9a9e083d70f53cb3603/README.md
FROM alpine-elixir-phoenix as phx-builder

# Set env
ENV MIX_ENV=dev

# Install temporary build deps
RUN apk --no-cache --update add \
      automake autoconf libtool nasm

# Cache elixir deps
ADD mix.exs mix.lock ./
RUN mix do deps.get, deps.compile

# Same with npm deps
ADD assets/package.json assets/package-lock.json assets/
RUN cd assets && \
    npm install --no-optional

# Now that everything is built/installed, reset back to the image we want to
# be running things in
FROM alpine-elixir-phoenix

# Set exposed ports and env
EXPOSE 4000
EXPOSE 8080
ENV PORT=4000 MIX_ENV=dev

# Copy deps over
COPY --from=phx-builder /opt/app/assets/node_modules /opt/app/assets/node_modules
# Make node_modules a volume so that it doesn't get replaced with host bind mount
VOLUME /opt/app/assets/node_modules
COPY --from=phx-builder /opt/app/_build /opt/app/_build
COPY --from=phx-builder /opt/app/deps /opt/app/deps
COPY --from=phx-builder /opt/app/.mix /opt/app/.mix
COPY --from=phx-builder /opt/app/mix.* /opt/app/

ADD . .

CMD ["mix", "phx.server"]