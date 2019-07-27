echo -e "Phase 3: App Setup"

# start in the /vagrant dir
cd /vagrant

# set mix setting to install hex non-interactively

mix local.hex --force

# get dependencies

mix deps.get

## DB

# set mix setting to install rebar3 non-interactively

mix local.rebar --force 

# create DB

mix ecto.create

# migrate DB

mix ecto.migrate

# node modules installation

cd assets
npm install
