# Orcasite

This repo specifies the web backend and frontend for Orcasound's whale listening. The backend is an [Elixir](https://elixir-lang.org/) app using the [Phoenix framework](https://phoenixframework.org/). The phoenix app serves a React app.

## Frontend

The frontend is built using React.

### Config

There's are extra env-based js config files not included in the repo for adding API keys. For the moment, the contents are simply:

js```
// assets/src/config/development.js

module.exports = {
  env: "development",
  development: true
}
```

js```
// assets/src/config/production.js

module.exports = {
  env: "production",
  development: false
}
```
