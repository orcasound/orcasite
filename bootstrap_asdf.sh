#!/usr/bin/env bash

echo -e 'Phase 2 - asdf installations...'
# Add Erlang using asdf
asdf plugin-add erlang
asdf install erlang 21.0.4
asdf global erlang 21.0.4

# # Add Elixir using asdf
asdf plugin-add elixir
asdf install elixir 1.7.1
asdf global elixir 1.7.1

# Add node.js using asdf
asdf plugin-add nodejs

# fix issue installing node
~/.asdf/plugins/nodejs/bin/import-release-team-keyring

asdf install nodejs 10.4.0
asdf global nodejs 10.4.0