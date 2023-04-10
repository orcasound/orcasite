#!/usr/bin/env bash

echo -e 'Phase 2 - asdf installations...'
# Add Erlang using asdf
asdf plugin-add erlang
asdf install erlang 22.3.4.26
asdf global erlang 22.3.4.26

# # Add Elixir using asdf
asdf plugin-add elixir
asdf install elixir 1.9.1-otp-22
asdf global elixir 1.9.1-otp-22

# Add node.js using asdf
asdf plugin-add nodejs

# fix issue installing node
~/.asdf/plugins/nodejs/bin/import-release-team-keyring

asdf install nodejs 12.9.1
asdf global nodejs 12.9.1
